use std::time::Duration;

pub fn join<'a, T: 'a, F>(array: impl Iterator<Item = T>, f: F) -> String
where
    F: FnMut(T) -> String,
{
    array.map(f).collect::<Vec<String>>().join(", ")
}

pub const fn read_u16(ins: &[u8]) -> u16 {
    ((ins[0] as u16) << 8) | (ins[1] as u16)
}

pub fn write_u16(ins: &mut [u8], value: u64) {
    ins[0] = (value >> 8) as u8;
    ins[1] = value as u8;
}

pub fn fmt_duration(duration: Duration) -> String {
    if duration.as_secs() > 1 {
        format!("{}s", duration.as_secs_f64())
    } else if duration.as_millis() > 1 {
        format!("{}ms", duration.as_millis())
    } else {
        format!("{}µs", duration.as_micros())
    }
}
