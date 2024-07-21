#[inline(always)]
pub fn err_stop<R, F: FnOnce() -> Result<R, loga::Error>>(f: F) -> Result<R, loga::Error> {
    f()
}

#[macro_export]
macro_rules! es{
    ($b: expr) => {
        $crate:: utils:: err_stop(|| $b)
    }
}
