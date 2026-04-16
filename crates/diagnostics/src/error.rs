#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    /// a byte that doesnt match any known token
    UnknownByte { byte: u8 },

    /// a number literal without digits (0x, 0b)
    EmptyNumber,

    /// a trailing decimal/exponent (1e, 1.)
    TrailingDecimal,
}
