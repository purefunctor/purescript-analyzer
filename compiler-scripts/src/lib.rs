pub use console;

pub mod fixtures {
    use md5::{Digest, Md5};
    use std::collections::HashMap;
    use std::fs;
    use std::path::Path;
    use walkdir::WalkDir;

    /// Hash all .purs files in a fixture directory (folder names + file contents)
    pub fn hash_fixtures(dir: &Path) -> String {
        let mut hasher = Md5::new();

        let Ok(entries) = fs::read_dir(dir) else {
            return String::new();
        };

        let mut folders: Vec<_> = entries.filter_map(|e| e.ok()).collect();
        folders.sort_by_key(|e| e.file_name());

        for folder in folders {
            let folder_path = folder.path();
            if !folder_path.is_dir() {
                continue;
            }

            // Hash folder name
            hasher.update(folder.file_name().to_string_lossy().as_bytes());

            // Hash all .purs files in the folder
            let mut purs_files: Vec<_> = WalkDir::new(&folder_path)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| e.path().extension().is_some_and(|ext| ext == "purs"))
                .collect();
            purs_files.sort_by_key(|e| e.path().to_path_buf());

            for file in purs_files {
                if let Ok(content) = fs::read(file.path()) {
                    hasher.update(&content);
                }
            }
        }

        let result = hasher.finalize();
        format!("{:x}", result)[..16].to_string()
    }

    /// Get environment variables with hashes for all fixture directories
    pub fn fixture_env() -> HashMap<String, String> {
        let base = Path::new("tests-integration/fixtures");
        HashMap::from([
            ("LSP_FIXTURES_HASH".into(), hash_fixtures(&base.join("lsp"))),
            ("LOWERING_FIXTURES_HASH".into(), hash_fixtures(&base.join("lowering"))),
            ("RESOLVING_FIXTURES_HASH".into(), hash_fixtures(&base.join("resolving"))),
            ("CHECKING_FIXTURES_HASH".into(), hash_fixtures(&base.join("checking"))),
        ])
    }
}

pub mod snapshots {
    use console::style;
    use similar::{ChangeTag, TextDiff};

    /// Strip insta frontmatter (YAML between --- markers) from snapshot content
    pub fn strip_frontmatter(content: &str) -> &str {
        let lines: Vec<&str> = content.lines().collect();
        if lines.first() != Some(&"---") {
            return content;
        }
        if let Some(end_idx) = lines.iter().skip(1).position(|&l| l == "---") {
            let start_byte: usize = lines[..end_idx + 2].iter().map(|l| l.len() + 1).sum();
            if start_byte <= content.len() {
                return &content[start_byte..];
            }
        }
        content
    }

    /// Print a colored diff between two strings with 2 lines of context
    pub fn print_diff(old: &str, new: &str) {
        let diff = TextDiff::from_lines(old, new);
        let groups = diff.grouped_ops(2);

        for (group_idx, group) in groups.iter().enumerate() {
            if group_idx > 0 {
                println!("{}", style("  ···").dim());
            }

            for op in group {
                for change in diff.iter_changes(op) {
                    let line_no = match change.tag() {
                        ChangeTag::Delete => change.old_index().map(|i| i + 1),
                        ChangeTag::Insert => change.new_index().map(|i| i + 1),
                        ChangeTag::Equal => change.new_index().map(|i| i + 1),
                    };
                    let line_no_str =
                        line_no.map(|n| format!("{:3}", n)).unwrap_or_else(|| "   ".into());

                    match change.tag() {
                        ChangeTag::Delete => {
                            print!("{}", style(format!("{} -{}", line_no_str, change)).red())
                        }
                        ChangeTag::Insert => {
                            print!("{}", style(format!("{} +{}", line_no_str, change)).green())
                        }
                        ChangeTag::Equal => {
                            print!("{}", style(format!("{}  {}", line_no_str, change)).dim())
                        }
                    }
                }
            }
        }
    }
}
