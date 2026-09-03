use emacs::{defun, Env, IntoLisp, Result, Value};
use lazy_static::lazy_static;
use nucleo_matcher::{
  pattern::{Atom, AtomKind, CaseMatching, Normalization},
  Matcher, Utf32Str,
};
use std::{
  iter,
  sync::{Mutex, PoisonError},
};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "fuzzy-matcher")]
fn init(_: &Env) -> Result<()> {
  Ok(())
}

lazy_static! {
  // Neither of these holds an invariant that a panic could leave broken, so
  // recover from poisoning rather than making one panic break every later
  // call.  A panic inside the module surfaces as a `rust-panic' lisp error.
  static ref MATCHER: Mutex<Matcher> = Matcher::default().into();
  // `fuzzy_indices' is called once per candidate with the same pattern, and
  // building an atom allocates, so keep the last one around.
  static ref ATOM: Mutex<Option<(String, Atom)>> = Mutex::new(None);
}

#[defun]
fn fuzzy_indices<'a>(env: &'a Env, pattern: Value<'a>, source: Value<'a>) -> Result<Option<Value<'a>>> {
  // Emacs can pass strings that aren't valid utf-8 (bug#74922); the fork of
  // emacs-module-rs reports those as an error rather than panicking, so a
  // single such candidate just doesn't match.
  let Ok(pattern) = pattern.into_rust::<String>() else {
    return Ok(None);
  };
  let Ok(source) = source.into_rust::<String>() else {
    return Ok(None);
  };
  let mut indices = Vec::new();
  let mut source_buf = Vec::new();
  let source = Utf32Str::new(&source, &mut source_buf);
  let mut cached_atom = ATOM.lock().unwrap_or_else(PoisonError::into_inner);
  if cached_atom.as_ref().is_none_or(|(cached, _)| *cached != pattern) {
    // Smart case: an all lower case pattern matches case insensitively, any
    // upper case character makes the match case sensitive.  Going through
    // `Atom' rather than calling `Matcher::fuzzy_indices' directly is what
    // applies the case folding and unicode normalization the matcher requires
    // its needle to already have.
    let atom = Atom::new(
      &pattern,
      CaseMatching::Smart,
      Normalization::Smart,
      AtomKind::Fuzzy,
      false,
    );
    *cached_atom = Some((pattern, atom));
  }
  let atom = &cached_atom.as_ref().unwrap().1;
  let mut matcher = MATCHER.lock().unwrap_or_else(PoisonError::into_inner);
  if let Some(score) = atom.indices(source, &mut matcher, &mut indices) {
    let indices = iter::once(score.into_lisp(env))
      .chain(indices.into_iter().map(|i| i.into_lisp(env)))
      .collect::<Result<Vec<Value>>>()?;
    env.list(&indices[..]).map(Some)
  } else {
    Ok(None)
  }
}
