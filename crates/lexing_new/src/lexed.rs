use std::sync::Arc;

use syntax::SyntaxKind;

/// Information attached to a [`SyntaxKind`].
///
/// [`SyntaxKind`] by itself does not store metadata, as it's represented by
/// a [`u16`]. In particular, this data type encapsulates offset information
/// used in post-lexing and parsing.
///
/// ## Invariants
///
/// The following invariants must hold for a [`SyntaxKindInfo`]:
///
/// * [`annotation`] is less than or equal to [`qualifier`].
/// * [`qualifier`] is less than or equal to [`token`].
/// * [`annotation`] is less than or equal to [`token`].
///
/// Refer to the documentation of each field for more invariants.
///
/// [`annotation`]: SyntaxKindInfo::annotation
/// [`qualifier`]: SyntaxKindInfo::qualifier
/// [`token`]: SyntaxKindInfo::token
#[derive(Debug, Clone, Copy)]
pub struct SyntaxKindInfo {
    /// The ending offset of a token's annotation.
    ///
    /// Annotations are either whitespace or comments attached as a prefix to a
    /// specific token. Rather than storing them as separate tokens, we simply
    /// attach them as metadata to a token to greatly simplify parsing.
    ///
    /// To obtain the textual content of an annotation, we use the [`token`]
    /// field of the previous token as the start offset and this field as the
    /// end offset.
    ///
    /// [`token`]: SyntaxKindInfo::token
    pub annotation: u32,
    /// The ending offset of a token's qualifier.
    ///
    /// Qualifiers are module names that identify the namespace of a token.
    /// They are usually present for name tokens such as [`SyntaxKind::LOWER`],
    /// and in special features such as qualified [`SyntaxKind::DO`].
    ///
    /// To obtain the textual content of a qualifier, we use the [`annotation`]
    /// as the start offset and this field as the end offset. Consequently, if
    /// the [`annotation`] is equal to this field, then there is no qualifier.
    ///
    /// [`annotation`]: SyntaxKindInfo::annotation
    pub qualifier: u32,
    /// The ending offset of a token's annotation.
    ///
    /// To obtain the textual content of a token, we use the [`qualifier`]
    /// as the start offset and this field as the end offset. Consequently, if
    /// the [`qualifier`] is equal to this field, then the token is zero-width.
    ///
    /// [`qualifier`]: SyntaxKindInfo::qualifier
    pub token: u32,
}

#[derive(Debug)]
struct LexerError {
    message: Arc<str>,
    index: u32,
}

pub struct Lexed<'s> {
    source: &'s str,
    kinds: Vec<SyntaxKind>,
    infos: Vec<SyntaxKindInfo>,
    errors: Vec<LexerError>,
}

impl<'s> Lexed<'s> {
    pub(crate) fn new(source: &'s str) -> Lexed<'s> {
        let kinds = vec![];
        let infos = vec![];
        let errors = vec![];
        Lexed { source, kinds, infos, errors }
    }

    pub(crate) fn push(&mut self, kind: SyntaxKind, info: SyntaxKindInfo, error: Option<&str>) {
        self.kinds.push(kind);
        self.infos.push(info);

        if let Some(error) = error {
            let message = error.into();
            let index = self.kinds.len() as u32 - 1;
            self.errors.push(LexerError { message, index });
        }
    }
}

impl<'s> Lexed<'s> {
    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.kinds.len());
        self.kinds[index]
    }

    pub fn info(&self, index: usize) -> SyntaxKindInfo {
        assert!(index < self.infos.len());
        self.infos[index]
    }

    pub fn annotation(&self, index: usize) -> Option<&str> {
        assert!(index < self.infos.len());

        let low = if index > 0 { self.infos[index - 1].token as usize } else { 0 };
        let high = self.infos[index].annotation as usize;

        if low < high {
            Some(&self.source[low..high])
        } else {
            None
        }
    }

    pub fn qualifier(&self, index: usize) -> Option<&str> {
        assert!(index < self.infos.len());

        let low = self.infos[index].annotation as usize;
        let high = self.infos[index].qualifier as usize;

        if low < high {
            Some(&self.source[low..high])
        } else {
            None
        }
    }

    pub fn position(&self, index: usize) -> (usize, usize) {
        assert!(index < self.infos.len());

        let info = self.infos[index];
        let token_start = info.annotation.min(info.qualifier) as usize;

        let haystack = &self.source[..token_start];
        let search = memchr::memchr_iter(b'\n', haystack.as_bytes());

        let line = search.count() + 1;
        let column = haystack.chars().rev().take_while(|&c| c != '\n').count() + 1;

        (line, column)
    }

    pub fn error(&self, index: usize) -> Option<Arc<str>> {
        assert!(index < self.kinds.len());
        let token_index = index as u32;
        let error_index = self.errors.binary_search_by_key(&token_index, |v| v.index).ok()?;
        Some(Arc::clone(&self.errors[error_index].message))
    }

    pub fn text(&self, index: usize) -> &str {
        assert!(index < self.infos.len());

        let low = self.infos[index].qualifier as usize;
        let high = self.infos[index].token as usize;

        &self.source[low..high]
    }

    pub fn len(&self) -> usize {
        self.kinds.len()
    }
}
