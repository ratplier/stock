#[macro_export]
/// Defines enum of keywords and implements conversion from and to &str.
///
/// Usage:
/// ```rust
/// define_keywords! {
///     "if" => If,
///     "else" => Else,
/// }
/// ```
macro_rules! define_keywords {
    (
        $( $string:literal => $variant:ident ),* $(,)?
    ) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        pub enum Keyword {
            $( $variant ),*
        }

        impl Keyword {
            pub fn as_str(&self) -> &'static str {
                match self {
                    $( Keyword::$variant => $string, )*
                }
            }
        }

        impl std::str::FromStr for Keyword {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $( $string => Ok(Keyword::$variant), )*
                    _ => Err(()),
                }
            }
        }
    };
}
