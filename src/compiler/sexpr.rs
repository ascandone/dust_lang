#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SExpr {
    Number(i64),
    Symbol(String),
    Bool(bool),
    String(String),
    List(Vec<SExpr>),
}
