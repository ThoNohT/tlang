/// Extensions on the Option type.
pub trait OptExt<T> {
    /// Assert that the provided value is Some, or run the code in on_error.
    /// This code should return the same type, but this may be because it never returns.
    fn assert_some(self: Self, on_error: fn() -> T) -> T;

    /// Returns the Option if it is Some, but also runs the function is it is None.
    fn or_do<'a>(self: Self, actn: impl FnOnce() -> ()) -> Self;

    /// Monadic bind.
    fn bind<R>(self: Self, f: impl FnOnce(T) -> Option<R>) -> Option<R>;
}

impl<T> OptExt<T> for Option<T> {
    fn assert_some(self: Self, on_error: fn() -> T) -> T {
        if let Some(v) = self {
            v
        } else {
            on_error()
        }
    }

    fn or_do<'a>(self: Self, actn: impl FnOnce() -> ()) -> Self {
        if self.is_none() {
            actn();
        }
        self
    }

    fn bind<R>(self, f: impl FnOnce(T) -> Option<R>) -> Option<R> {
        match self {
            Some(v) => f(v),
            None => None,
        }
    }
}
