import pako from "pako";
import type { RawModule, PackageSet } from "./types";

const REGISTRY_URL = "https://packages.registry.purescript.org";
const PACKAGE_SET_URL =
  "https://raw.githubusercontent.com/purescript/package-sets/master/packages.json";

/**
 * Parse tar archive and extract .purs files.
 * Tar format: 512-byte headers followed by file content (padded to 512).
 */
function parseTar(data: Uint8Array): Map<string, string> {
  const files = new Map<string, string>();
  const decoder = new TextDecoder("utf-8");
  let offset = 0;

  while (offset < data.length - 512) {
    // Read header (512 bytes)
    const header = data.slice(offset, offset + 512);

    // Check for empty block (end of archive)
    if (header.every((b) => b === 0)) break;

    // Parse filename (bytes 0-99, null-terminated)
    const nameBytes = header.slice(0, 100);
    const nameEnd = nameBytes.indexOf(0);
    const name = decoder.decode(nameBytes.slice(0, nameEnd > 0 ? nameEnd : 100));

    // Parse file size (bytes 124-135, octal string)
    const sizeStr = decoder.decode(header.slice(124, 136)).replace(/\0/g, "").trim();
    const size = parseInt(sizeStr, 8) || 0;

    // Parse type flag (byte 156): '0' or '\0' = regular file
    const typeFlag = header[156];
    const isFile = typeFlag === 0 || typeFlag === 48; // 48 = '0'

    offset += 512; // Move past header

    if (isFile && size > 0) {
      const content = data.slice(offset, offset + size);

      // Only extract .purs files from src/ directory
      if (name.endsWith(".purs") && name.includes("/src/")) {
        const source = decoder.decode(content);
        files.set(name, source);
      }
    }

    // Move to next header (content padded to 512-byte boundary)
    offset += Math.ceil(size / 512) * 512;
  }

  return files;
}

export async function fetchPackage(
  packageName: string,
  version: string,
  onProgress?: (progress: number) => void
): Promise<RawModule[]> {
  // Strip 'v' prefix from version for registry URL
  const versionNum = version.startsWith("v") ? version.slice(1) : version;
  const url = `${REGISTRY_URL}/${packageName}/${versionNum}.tar.gz`;

  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch ${packageName}@${version}: ${response.status}`);
  }

  const contentLength = response.headers.get("content-length");
  const total = contentLength ? parseInt(contentLength, 10) : 0;

  // Read with progress tracking
  const reader = response.body!.getReader();
  const chunks: Uint8Array[] = [];
  let received = 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    received += value.length;
    if (total && onProgress) {
      onProgress(received / total);
    }
  }

  // Combine chunks
  const compressed = new Uint8Array(received);
  let position = 0;
  for (const chunk of chunks) {
    compressed.set(chunk, position);
    position += chunk.length;
  }

  // Decompress gzip
  const tarData = pako.ungzip(compressed);

  // Extract .purs files
  const files = parseTar(tarData);

  // Convert to raw modules (path + source, no module name parsing)
  const modules: RawModule[] = [];
  for (const [path, source] of files) {
    modules.push({ path, source });
  }

  return modules;
}

export async function fetchPackageSet(): Promise<PackageSet> {
  const response = await fetch(PACKAGE_SET_URL);
  if (!response.ok) {
    throw new Error(`Failed to fetch package set: ${response.status}`);
  }
  return response.json();
}
