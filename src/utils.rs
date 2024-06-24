pub fn join<'a, T: 'a, F>(array: impl Iterator<Item = T>, f: F) -> String
where
    F: FnMut(T) -> String,
{
    array.map(f).collect::<Vec<String>>().join(", ")
}
