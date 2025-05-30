fn main() {
    println!("cargo:rerun-if-changed=tests/layout");
    println!("cargo:rerun-if-changed=tests/lexer");
}
