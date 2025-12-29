import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import axios from "axios";
import * as tar from "tar";

const PACKAGE_SET_URL =
  "https://raw.githubusercontent.com/purescript/registry/refs/heads/main/package-sets/64.7.1.json";

const CORE_PACKAGES_PATH = join(import.meta.dirname, "purescript-core.json");
const PACKAGE_TARBALLS = join(import.meta.dirname, "packages", "_downloads");

const MAX_CONCURRENT_DOWNLOADS = 8;
const DELAY_BETWEEN_BATCHES_MS = 1000;

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function downloadInBatches(items, batchSize, delayMs, downloadFn) {
  for (let i = 0; i < items.length; i += batchSize) {
    const batch = items.slice(i, i + batchSize);
    await Promise.all(batch.map(downloadFn));
    if (i + batchSize < items.length) {
      await sleep(delayMs);
    }
  }
}

async function corePackages() {
  // Read the list of core packages (with purescript- prefix)
  const coreList = JSON.parse(readFileSync(CORE_PACKAGES_PATH, "utf-8"));
  const coreSet = new Set(coreList);

  // Fetch the package set to get versions
  const response = await axios.get(PACKAGE_SET_URL);
  const allPackages = response.data.packages;

  // Filter to only core packages (registry uses names without purescript- prefix)
  const filtered = Object.entries(allPackages)
    .filter(([name]) => coreSet.has(`purescript-${name}`))
    .map(([name, version]) => `${name}/${version}.tar.gz`);

  console.log(`Found ${filtered.length} core packages out of ${Object.keys(allPackages).length} total`);
  return filtered;
}

async function downloadPackages() {
  const corePackagesList = await corePackages();
  mkdirSync(PACKAGE_TARBALLS, { recursive: true });

  const toDownload = corePackagesList.filter((registryPackage) => {
    const packageFile = join(PACKAGE_TARBALLS, dirname(registryPackage));
    if (existsSync(`${packageFile}.tar.gz`)) {
      console.log(`${packageFile}.tar.gz already exists, skipping.`);
      return false;
    }
    return true;
  });

  console.log(`Downloading ${toDownload.length} packages...`);

  await downloadInBatches(
    toDownload,
    MAX_CONCURRENT_DOWNLOADS,
    DELAY_BETWEEN_BATCHES_MS,
    async (registryPackage) => {
      const packageUrl = `https://packages.registry.purescript.org/${registryPackage}`;
      const packageFile = join(PACKAGE_TARBALLS, dirname(registryPackage));
      console.log(`Downloading ${packageUrl}`);
      const response = await axios.get(packageUrl, {
        responseType: "arraybuffer",
      });
      writeFileSync(`${packageFile}.tar.gz`, response.data);
    }
  );
}

async function extractSourceFiles() {
  const packageFiles = readdirSync(PACKAGE_TARBALLS).filter((file) =>
    file.endsWith(".tar.gz")
  );

  const cwd = join(PACKAGE_TARBALLS, "..");
  const filter = (p) => p.endsWith(".purs");

  await Promise.all(
    packageFiles.map(async (packageFile) => {
      const file = join(PACKAGE_TARBALLS, packageFile);
      console.log(`Extracting ${file}`);
      tar.x({ file, cwd, filter });
    })
  );
}

async function main() {
  await downloadPackages();
  await extractSourceFiles();
}

main().catch(console.error);
