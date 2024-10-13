#[no_mangle]
pub extern "C" fn _start() {
    main();
}

fn main() {
    println!("Hello, world!");
}
