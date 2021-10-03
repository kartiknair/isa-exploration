#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
    // While offset could be derived from line/column it's convenient to have it directly
    pub offset: usize,
    pub length: usize,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub pos: Pos,
    pub message: String,
}
