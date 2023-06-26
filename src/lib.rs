// re-exports for macro
#[doc(hidden)]
pub use anyhow;
#[doc(hidden)]
pub use serde;

/// Macro for conveniently creating a "newtype" tuple struct with a certain
/// validation logic.
///
/// Consider this example:
/// ```rust,no_compile
/// # use serde_newtype::newtype;
/// newtype! {
///     MyNewtype: u64[|num|
///         true,
///         "invalid"
///     ] = 0;
/// }
/// ```
///
/// A struct, `MyNewtype(u64)`, will be generated with the following methods and
/// trait implementations:
/// - `MyNewtype::new() -> anyhow::Result<u64>`
/// - `MyNewtype::get(self) -> u64
/// - `impl Deref<Target = u64> for MyNewtype`
/// - `impl TryFrom<u64, Error = anyhow::Error>> for MyNewtype`
/// - `impl From<MyNewtype> for u64`
/// - `impl serde::Serialize for MyNewtype`
/// - `impl<'de> serde::Deserialize<'de> for MyNewtype`
/// - `impl Default for MyNewtype` (unless default value is omitted)
///
/// ## Syntax
/// ```text
/// Name: Inner[|bind|
///     validity_check(bind),
///     "error message: got {bind}, expected {}",
///     expected,
/// ] = default_value();
/// ```
///
/// The validation part is similar in its syntax to a closure, it takes the name
/// to bind to a `&Inner` inside the validation and error expressions.
///
/// The first expression inside the "closure" should be an expression that
/// evaluates to `true` if the value is valid, and false otherwise.
/// One more expressions should follow, and are passed verbatim into the
/// [`anyhow!`](anyhow::anyhow) macro to create an [`Error`](anyhow::Error) when
/// validation fails.
///
/// The default value is optional, and is *also validated*
/// using the validation expression. If an invalid value is produced by the
/// default expression, then calling `<Name as Default>::default()` will panic!
///
/// Multiple newtypes can be specified within the same macro invocation.
///
/// ## Example
/// ```
/// use serde_newtype::newtype;
/// use serde::{Deserialize, Serialize};
/// use serde_json::json;
///
/// newtype! {
///     #[derive(Debug)]
///     Even: u64[|num|
///         num % 2 == 0,
///         "number must be even, got {num}",
///     ];
/// }
///
/// #[derive(Debug, Deserialize, Serialize)]
/// struct A {
///     even: Even,
/// }
///
/// // Deserialization requires the value to be valid:
/// assert_eq!(
///     serde_json::from_value::<A>(json!({ "even": 2 }))
///         .unwrap()
///         .even
///         .get(),
///     2
/// );
/// assert!(serde_json::from_value::<A>(json!({ "even": 1 }))
///     .unwrap_err()
///     .to_string()
///     .contains("number must be even, got 1"));
///
/// // Creating an `Even` programmatically still requires going through validation:
/// assert_eq!(Even::new(2).unwrap().get(), 2);
/// assert!(Even::new(1).is_err());
/// ```
#[macro_export]
macro_rules! newtype {
    {
        $(#[$meta:meta])*
        $vis:vis $name:ident: $ty:ty[
            |$val_arg:ident|
            $validate:expr,
            $($err_param:expr),+$(,)?
        ]$( = $default:expr)?;
    } => {
        $(#[$meta])*
        $vis struct $name($ty);

        impl $name {
            $vis fn new(val: $ty) -> ::std::result::Result<Self, $crate::anyhow::Error> {
                let $val_arg = &val;
                if $validate {
                    Ok(Self(val))
                } else {
                    Err($crate::anyhow::anyhow!($($err_param),+))
                }
            }

            #[inline]
            $vis fn get(self) -> $ty {
                self.0
            }
        }

        impl ::std::ops::Deref for $name {
            type Target = $ty;

            #[inline]
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        $(
            impl ::std::default::Default for $name {
                fn default() -> Self {
                    use $crate::anyhow::Context;

                    Self::new($default)
                        .with_context(|| format!("Invalid default value for {}", ::std::any::type_name::<Self>()))
                        .unwrap()
                }
            }
        )?

        impl $crate::serde::Serialize for $name {
            #[inline]
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error>
                where S: $crate::serde::Serializer,
            {
                <$ty as $crate::serde::Serialize>::serialize(&self.0, serializer)
            }
        }

        impl<'de> $crate::serde::Deserialize<'de> for $name {
            #[inline]
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error>
            where
                D: $crate::serde::Deserializer<'de>,
            {
                let val = <$ty as $crate::serde::Deserialize>::deserialize(deserializer)?;
                let val = Self::new(val).map_err(<D::Error as $crate::serde::de::Error>::custom)?;
                Ok(val)
            }
        }

        impl ::std::convert::TryFrom<$ty> for $name {
            type Error = $crate::anyhow::Error;

            #[inline]
            fn try_from(value: $ty) -> Result<Self, Self::Error> {
                Self::new(value)
            }
        }

        impl ::std::convert::From<$name> for $ty {
            #[inline]
            fn from(value: $name) -> Self {
                value.get()
            }
        }
    };
    {$(
        $(#[$meta:meta])*
        $vis:vis $name:ident: $ty:ty[
            |$val_arg:ident|
            $validate:expr,
            $($err_param:expr),+
        ]$( = $default:expr)?;
    )+} => {
        $(
            newtype! {
                $(#[$meta])*
                $vis $name: $ty[|$val_arg| $validate, $($err_param),+]$( = $default)?;
            }
        )+
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Invalid default value")]
    fn test_newtype_invalid_default() {
        newtype! {
            #[derive(Debug)]
            Even: u64[|num|
                num % 2 == 0,
                "number must be even, got {num}",
            ] = 1 /* this default value is invalid */;
        }

        let _ = Even::default();
    }
}
