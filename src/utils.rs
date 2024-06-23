pub fn join<T: ToString>(array: &[T]) -> String {
    array
        .iter()
        .map(|expr| expr.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}
