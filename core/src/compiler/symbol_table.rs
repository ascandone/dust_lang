use crate::ast::{Ident, Namespace};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Scope {
    Global(u16),
    Local(u8),
    Free(u8),
    Function,
}

#[derive(Debug)]
struct LocalScope {
    pub idents: HashMap<String, Vec<u8>>,
    pub next_local: u8,
    pub free: Vec<Scope>,
    pub name: Option<String>,
}

impl LocalScope {
    pub fn get(&self, name: &str) -> Option<&u8> {
        let v_opt = self.idents.get(name);
        v_opt.map(|v| v.last().unwrap())
    }
}

impl LocalScope {
    fn new(name: Option<String>) -> Self {
        Self {
            idents: HashMap::new(),
            next_local: 0,
            free: vec![],
            name,
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
struct QualifiedIdent {
    pub ns: Namespace,
    pub name: String,
}

// TODO remove
#[derive(Debug)]
struct Global {
    id: u16,
    public: bool,
}

pub struct SymbolTable {
    globals: HashMap<QualifiedIdent, Global>,
    next_global: u16,
    locals: Vec<LocalScope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            globals: HashMap::new(),
            next_global: 0,

            locals: vec![LocalScope::new(None)],
        }
    }

    pub fn free(&self) -> Vec<Scope> {
        self.current_local().free.clone()
    }

    pub fn count_locals(&self) -> u8 {
        self.current_local().next_local
    }

    pub fn enter_scope(&mut self, name: Option<String>) {
        self.locals.push(LocalScope::new(name));
    }

    pub fn exit_scope(&mut self) {
        let res = self.locals.pop();

        if res.is_none() {
            panic!("Cannot pop from top-level scope")
        }
    }

    pub fn define_global(&mut self, public: bool, ns: &Namespace, unqualified_name: &str) -> u16 {
        let id = self.next_global;

        self.globals.insert(
            QualifiedIdent {
                ns: ns.clone(),
                name: unqualified_name.to_string(),
            },
            Global { id, public },
        );
        self.next_global += 1;
        id
    }

    pub fn define_local(&mut self, name: &str) -> u8 {
        let loc = self.current_local_mut();
        let ident = loc.next_local;

        let v = match loc.idents.get_mut(name) {
            None => {
                loc.idents.insert(name.to_string(), vec![]);
                loc.idents.get_mut(name).unwrap()
            }
            Some(v) => v,
        };

        v.push(ident);

        loc.next_local += 1;
        ident
    }

    pub fn remove_local(&mut self, name: &str) {
        let loc = self.current_local_mut().idents.get_mut(name);
        if let Some(v) = loc {
            v.pop();
            if v.is_empty() {
                self.current_local_mut().idents.remove(name);
            }
        }
    }

    fn current_local(&self) -> &LocalScope {
        self.locals.last().unwrap()
    }

    fn current_local_mut(&mut self) -> &mut LocalScope {
        self.locals.last_mut().unwrap()
    }

    fn resolve_free(&mut self, name: &str, index: usize) -> Option<u8> {
        if index <= 1 {
            return None;
        }

        let lookup = self.locals[index - 2].get(name);

        let mut todos = vec![];
        let res = match lookup {
            None => {
                let nested_lookup = self.resolve_free(name, index - 1);

                if let Some(ident) = nested_lookup {
                    todos.push((index - 1, Scope::Free(ident)));
                };

                nested_lookup
            }
            Some(ident) => {
                // TODO refactor this ugly control flow
                // check if already pushed to free
                if self.locals[index - 1]
                    .free
                    .iter()
                    .any(|scope| scope == &Scope::Local(*ident))
                {
                    return Some(*ident);
                }

                todos.push((index - 1, Scope::Local(*ident)));
                Some(self.locals[index - 1].free.len() as u8)
            }
        };

        for (index, scope) in todos {
            self.locals[index].free.push(scope);
        }

        res
    }

    pub fn resolve(
        &mut self,
        current_ns: &Namespace,
        Ident(ident_ns, unqualified_name): &Ident,
    ) -> Option<Scope> {
        // TODO handle blank identifiers at a parsing+ast level
        if unqualified_name == "_" {
            return None;
        }

        if let Some(ident) = &self.current_local().get(unqualified_name) {
            return Some(Scope::Local(**ident));
        }

        if let Some(local_name) = &self.current_local().name {
            if unqualified_name == local_name {
                return Some(Scope::Function);
            }
        }

        if let Some(ident) = self.resolve_free(unqualified_name, self.locals.len()) {
            return Some(Scope::Free(ident));
        }

        let ident_ns = ident_ns.clone().unwrap_or(current_ns.clone());

        if let Some(Global { id, public }) = self.globals.get(&QualifiedIdent {
            ns: ident_ns.clone(),
            name: unqualified_name.to_string(),
        }) {
            if *public || &ident_ns == current_ns {
                return Some(Scope::Global(*id));
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn main_ns() -> Namespace {
        Namespace(vec!["Main".to_string()])
    }

    #[test]
    fn test_define_global() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(false, &main_ns(), &"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(0))
        )
    }

    #[test]
    fn test_shadowing_global() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(false, &main_ns(), &"x".to_string());
        symbol_table.define_global(false, &main_ns(), &"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(1))
        )
    }

    #[test]
    fn test_shadowing_global_twice() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(false, &main_ns(), &"x".to_string());
        symbol_table.define_global(false, &main_ns(), &"x".to_string());
        symbol_table.define_global(false, &main_ns(), &"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(2))
        )
    }

    #[test]
    fn test_define_global_resolve_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_global(false, &main_ns(), &"x".to_string());
        symbol_table.enter_scope(None);

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(0))
        )
    }

    #[test]
    fn test_define_global_in_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope(None);
        symbol_table.define_global(false, &main_ns(), &"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(0))
        );

        symbol_table.exit_scope();

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(0))
        );

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Global(0))
        );
    }

    #[test]
    fn test_define_local_toplevel() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Local(0))
        )
    }

    #[test]
    fn test_remove_local_top_level() {
        let x = "x".to_string();
        let y = "y".to_string();
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&x);
        symbol_table.define_local(&y);
        symbol_table.remove_local(&x);

        assert_eq!(symbol_table.resolve(&main_ns(), &Ident(None, x)), None);
        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, y)),
            Some(Scope::Local(1))
        );
    }

    #[test]
    fn test_remove_shadowed() {
        let x = "x".to_string();

        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&x);
        symbol_table.define_local(&x);
        symbol_table.remove_local(&x);

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, x)),
            Some(Scope::Local(0))
        );
    }

    #[test]
    fn test_shadowing_local() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Local(1))
        )
    }

    #[test]
    fn test_define_local_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope(None);
        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Local(0))
        )
    }

    #[test]
    fn test_shadowing_global_local() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_global(false, &main_ns(), &"x".to_string());

        symbol_table.enter_scope(None);

        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Local(0))
        )
    }

    #[test]
    fn test_free() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope(None);

        let lookup = symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string()));
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }

    #[test]
    fn test_free_twice_same_variable() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope(None);

        let _ = symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string()));
        let lookup = symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string()));
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }

    #[test]
    fn test_free_twice() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"y".to_string());
        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope(None);

        let lookup_x = symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string()));
        let lookup_y = symbol_table.resolve(&main_ns(), &Ident(None, "y".to_string()));

        assert_eq!(lookup_x, Some(Scope::Free(0)));
        assert_eq!(lookup_y, Some(Scope::Free(1)));

        assert_eq!(
            &symbol_table.current_local().free,
            &vec![Scope::Local(1), Scope::Local(0)]
        );
    }

    #[test]
    fn test_free_nested_scope() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope(None);
        symbol_table.enter_scope(None);

        let lookup = symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string()));
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Free(0)]);

        symbol_table.exit_scope();
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }

    #[test]
    fn test_fn_scope() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope(Some("x".to_string()));

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Function)
        );

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "not-found".to_string())),
            None
        );

        symbol_table.exit_scope();
        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            None
        );
    }

    #[test]
    fn test_fn_scope_local_shadowing() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope(Some("x".to_string()));
        symbol_table.define_local("x");

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Local(0))
        );
    }

    #[test]
    fn test_fn_scope_global_now_shadowing() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope(Some("x".to_string()));
        symbol_table.define_global(false, &main_ns(), "x");

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(None, "x".to_string())),
            Some(Scope::Function)
        );
    }

    #[test]
    fn test_two_qualified_lookups() {
        let mut symbol_table = SymbolTable::new();

        let mod_a = Namespace(vec!["A".to_string()]);
        let mod_b = Namespace(vec!["B".to_string()]);

        symbol_table.define_global(true, &mod_a, "x");
        symbol_table.define_global(true, &mod_b, "x");

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(Some(mod_a.clone()), "x".to_string())),
            Some(Scope::Global(0)),
            "resolve A.x"
        );

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(Some(mod_b.clone()), "x".to_string())),
            Some(Scope::Global(1)),
            "resolve B.x"
        );
    }

    #[test]
    fn test_private_var() {
        let mut symbol_table = SymbolTable::new();

        let mod_a = Namespace(vec!["A".to_string()]);

        symbol_table.define_global(false, &mod_a, "x");

        assert_eq!(
            symbol_table.resolve(&main_ns(), &Ident(Some(mod_a.clone()), "x".to_string())),
            None,
        );
    }
}
