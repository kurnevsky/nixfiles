use emacs::{defun, Env, IntoLisp, Result, Value};
use lazy_static::lazy_static;
use nucleo_matcher::{Matcher, Utf32Str};
use std::{iter, sync::Mutex};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "fuzzy-matcher")]
fn init(_: &Env) -> Result<()> {
  Ok(())
}

lazy_static! {
  static ref MATCHER: Mutex<Matcher> = Matcher::default().into();
}

#[defun]
fn skim_fuzzy_indices(env: &Env, pattern: String, source: String) -> Result<Option<Value>> {
  let mut indices = Vec::new();
  let mut source_buf = Vec::new();
  let mut pattern_buf = Vec::new();
  let source = Utf32Str::new(&source, &mut source_buf);
  let pattern = Utf32Str::new(&pattern, &mut pattern_buf);
  if let Some(score) = MATCHER.lock().unwrap().fuzzy_indices(source, pattern, &mut indices) {
    let indices = iter::once(score.into_lisp(env))
      .chain(indices.into_iter().map(|i| i.into_lisp(env)))
      .collect::<Result<Vec<Value>>>()?;
    env.list(&indices[..]).map(Some)
  } else {
    Ok(None)
  }
}
