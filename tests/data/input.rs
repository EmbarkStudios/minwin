// Equivalent windows_sys imports
// use windows_sys::Win32::{
//     Foundation::{GetLastError, BOOL, ERROR_TIMEOUT},
//     System::{
//         LibraryLoader::{GetModuleHandleA, GetProcAddress},
//         WindowsProgramming::INFINITE,
//     },
// };

#[minwin]
mod example {
    const INFINITE: () = ();
    const ERROR_TIMEOUT: () = ();
    fn GetLastError();
    fn GetModuleHandleA();
    fn GetProcAddress();
    type WaitAddress = fn();
    type WakeByAddressSingle = fn();
}

use example::*;

fn main() {
    let synch_dll = unsafe { GetModuleHandleA(b"api-ms-win-core-synch-l1-2-0.dll\0".as_ptr()) };
    if synch_dll == 0 {
        panic!("oh no");
    }

    let wait = unsafe { GetProcAddress(synch_dll, b"WaitOnAddress\0".as_ptr())? };
    let wake = unsafe { GetProcAddress(synch_dll, b"WakeByAddressSingle\0".as_ptr())? };

    let wait: WaitAddress = unsafe { std::mem::transmute(wait) };
    let wake: WakeByAddressSingle = unsafe { std::mem::transmute(wake) };
}
