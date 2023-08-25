use emacs::{defun, Env, IntoLisp, Result, Value};
use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher};
use std::iter;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "fuzzy-matcher")]
fn init(_: &Env) -> Result<()> {
  Ok(())
}

#[defun]
fn skim_fuzzy_indices(env: &Env, pattern: String, source: String) -> Result<Option<Value>> {
  if let Some((score, indices)) = SkimMatcherV2::default().fuzzy_indices(&source, &pattern) {
    let indices = iter::once(score.into_lisp(env))
      .chain(indices.into_iter().map(|i| i.into_lisp(env)))
      .collect::<Result<Vec<Value>>>()?;
    env.list(&indices[..]).map(Some)
  } else {
    Ok(None)
  }
}
