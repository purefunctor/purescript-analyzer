fn main() {
    println!("cargo::rerun-if-changed=packages");
    if !std::fs::exists("packages").unwrap() {
        println!("cargo::warning=node download-registry.js");
    }
}
