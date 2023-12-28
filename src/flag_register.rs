pub enum ProcessorFlags {
    CF = 0b0000000000000001,
    ZF = 0b0000000000000010,
    OF = 0b0000000000000100,
    SF = 0b0000000000001000,
}

pub trait FlagRegister {
    fn set(&mut self, flag: ProcessorFlags);
    fn clear(&mut self, flag: ProcessorFlags);
    fn is_set(&self, flag: ProcessorFlags) -> bool;
    fn is_clear(&self, flag: ProcessorFlags) -> bool;
    fn reset(&mut self);

    #[inline]
    fn set_if(&mut self, condition: bool, flag: ProcessorFlags) {
        if condition {
            self.set(flag);
        } else {
            self.clear(flag);
        }
    }
}

macro_rules! implement_flag_register {
    ($name:ident($type:ty)) => {
        pub struct $name(pub $type);

        impl $name {
            fn new() -> $name {
                $name(0)
            }
        }

        impl FlagRegister for $name {
            #[inline]
            fn set(&mut self, flag: ProcessorFlags) {
                self.0 |= flag as $type;
            }

            #[inline]
            fn clear(&mut self, flag: ProcessorFlags) {
                self.0 &= !(flag as $type);
            }

            #[inline]
            fn is_set(&self, flag: ProcessorFlags) -> bool {
                self.0 & (flag as $type) != 0
            }

            #[inline]
            fn is_clear(&self, flag: ProcessorFlags) -> bool {
                !self.is_set(flag)
            }

            #[inline]
            fn reset(&mut self) {
                self.0 = 0;
            }
        }
    };
}

pub(crate) use implement_flag_register;
