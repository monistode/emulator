use super::common::ProcessorContinue;

#[inline]
pub fn halt() -> ProcessorContinue {
    ProcessorContinue::Halt
}
