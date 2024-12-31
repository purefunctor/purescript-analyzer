import { existsSync, mkdirSync, readdirSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import axios from "axios";
import * as tar from "tar";

const PACKAGE_SET_URL =
  "https://raw.githubusercontent.com/purescript/registry/refs/heads/main/package-sets/62.2.1.json";

const PACKAGE_TARBALLS = join(import.meta.dirname, "packages", "_downloads");

async function registryPackages() {
  const response = await axios.get(PACKAGE_SET_URL);
  const packages = response.data.packages;
  return Object.entries(packages).map(
    ([key, value]) => `${key}/${value}.tar.gz`
  );
}

async function downloadPackages() {
  const registryPackagesList = await registryPackages();
  mkdirSync(PACKAGE_TARBALLS, { recursive: true });

  await Promise.all(
    registryPackagesList.map(async (registryPackage) => {
      const packageUrl = `https://packages.registry.purescript.org/${registryPackage}`;
      const packageFile = join(PACKAGE_TARBALLS, dirname(registryPackage));
      if (existsSync(`${packageFile}.tar.gz`)) {
        return console.log(
          `${packageFile}.tar.gz already exists, skipping download.`
        );
      } else {
        console.log(`Downloading ${packageUrl}`);
      }
      const response = await axios.get(packageUrl, {
        responseType: "arraybuffer",
      });
      writeFileSync(`${packageFile}.tar.gz`, response.data);
    })
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
