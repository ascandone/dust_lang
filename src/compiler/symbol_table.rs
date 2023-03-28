#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Scope {
    Global(u16),
    Local(u8),
    Free(u8),
}

#[derive(Debug)]
struct LocalScope {
    pub idents: HashMap<String, Vec<u8>>,
    pub next_local: u8,
    pub free: Vec<Scope>,
}

impl LocalScope {
    pub fn get(&self, name: &str) -> Option<&u8> {
        let v_opt = self.idents.get(name);
        v_opt.map(|v| v.last().unwrap())
    }
}

impl LocalScope {
    fn new() -> Self {
        Self {
            idents: HashMap::new(),
            next_local: 0,
            free: vec![],
        }
    }
}

pub struct SymbolTable {
    globals: HashMap<String, u16>,
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

            locals: vec![LocalScope::new()],
        }
    }

    pub fn free(&self) -> Vec<Scope> {
        self.current_local().free.clone()
    }

    pub fn count_locals(&self) -> u8 {
        self.current_local().next_local
    }

    pub fn enter_scope(&mut self) {
        self.locals.push(LocalScope::new());
    }

    pub fn exit_scope(&mut self) {
        let res = self.locals.pop();

        if res.is_none() {
            panic!("Cannot pop from top-level scope")
        }
    }

    pub fn define_global(&mut self, name: &str) -> u16 {
        let ident = self.next_global;
        self.globals.insert(name.to_string(), ident);
        self.next_global += 1;
        ident
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

    pub fn resolve(&mut self, name: &str) -> Option<Scope> {
        if let Some(ident) = self.current_local().get(name) {
            return Some(Scope::Local(*ident));
        }

        if let Some(ident) = self.resolve_free(name, self.locals.len()) {
            return Some(Scope::Free(ident));
        }

        if let Some(ident) = self.globals.get(name) {
            return Some(Scope::Global(*ident));
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_global() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(0))
        )
    }

    #[test]
    fn test_shadowing_global() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(&"x".to_string());
        symbol_table.define_global(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(1))
        )
    }

    #[test]
    fn test_shadowing_global_twice() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_global(&"x".to_string());
        symbol_table.define_global(&"x".to_string());
        symbol_table.define_global(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(2))
        )
    }

    #[test]
    fn test_define_global_resolve_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_global(&"x".to_string());
        symbol_table.enter_scope();

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(0))
        )
    }

    #[test]
    fn test_define_global_in_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope();
        symbol_table.define_global(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(0))
        );

        symbol_table.exit_scope();

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(0))
        );

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Global(0))
        );
    }

    #[test]
    fn test_define_local_toplevel() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
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

        assert_eq!(symbol_table.resolve(&x), None);
        assert_eq!(symbol_table.resolve(&y), Some(Scope::Local(1)));
    }

    #[test]
    fn test_remove_shadowed() {
        let x = "x".to_string();

        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&x);
        symbol_table.define_local(&x);
        symbol_table.remove_local(&x);

        assert_eq!(symbol_table.resolve(&x), Some(Scope::Local(0)));
    }

    #[test]
    fn test_shadowing_local() {
        let symbol_table = &mut SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Local(1))
        )
    }

    #[test]
    fn test_define_local_nested() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.enter_scope();
        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Local(0))
        )
    }

    #[test]
    fn test_shadowing_global_local() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_global(&"x".to_string());

        symbol_table.enter_scope();

        symbol_table.define_local(&"x".to_string());

        assert_eq!(
            symbol_table.resolve(&"x".to_string()),
            Some(Scope::Local(0))
        )
    }

    #[test]
    fn test_free() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope();

        let lookup = symbol_table.resolve(&"x".to_string());
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }

    #[test]
    fn test_free_twice_same_variable() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope();

        let _ = symbol_table.resolve(&"x".to_string());
        let lookup = symbol_table.resolve(&"x".to_string());
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }

    #[test]
    fn test_free_twice() {
        let mut symbol_table = SymbolTable::new();

        symbol_table.define_local(&"y".to_string());
        symbol_table.define_local(&"x".to_string());
        symbol_table.enter_scope();

        let lookup_x = symbol_table.resolve(&"x".to_string());
        let lookup_y = symbol_table.resolve(&"y".to_string());

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
        symbol_table.enter_scope();
        symbol_table.enter_scope();

        let lookup = symbol_table.resolve(&"x".to_string());
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Free(0)]);

        symbol_table.exit_scope();
        assert_eq!(lookup, Some(Scope::Free(0)));
        assert_eq!(&symbol_table.current_local().free, &vec![Scope::Local(0)]);
    }
}
