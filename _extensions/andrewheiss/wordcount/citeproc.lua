-- Lua filter that behaves like `--citeproc` (the format sets `citeproc: false`,
-- so this filter is what actually resolves citations and builds the
-- bibliography).
--
-- It runs at the `pre-quarto` stage, i.e. before Quarto resolves
-- cross-references, so Quarto cross-references such as @fig-…, @tbl-…, @eq-…
-- are still plain citation nodes here. Passing them to citeproc makes it emit
-- spurious "citation … not found" warnings. To avoid that without losing the
-- cross-references, we hide cross-reference citations behind placeholders while
-- citeproc runs, then restore them so Quarto can resolve them in its own pass.

local crossref_prefixes = {
  "fig", "tbl", "eq", "lst", "sec", "thm", "lem", "cor", "prp", "cnj",
  "def", "exm", "exr", "sol", "rem", "fnref"
}

local function is_crossref(id)
  for _, p in ipairs(crossref_prefixes) do
    if id:match("^" .. p .. "%-") then return true end
  end
  return false
end

local function all_crossref(cite)
  for _, c in ipairs(cite.citations) do
    if not is_crossref(c.id) then return false end
  end
  return true
end

local MARK = "\u{FFFC}" -- object replacement character, safe through citeproc

function Pandoc (doc)
  local saved = {}
  local n = 0

  -- Hide pure cross-reference citations behind unique placeholders.
  doc = doc:walk{
    Cite = function (cite)
      if all_crossref(cite) then
        n = n + 1
        saved[n] = cite
        return pandoc.Str(MARK .. n .. MARK)
      end
      return cite
    end
  }

  doc = pandoc.utils.citeproc(doc)

  -- Restore the cross-reference citations for Quarto to resolve later.
  doc = doc:walk{
    Str = function (s)
      local idx = s.text:match("^" .. MARK .. "(%d+)" .. MARK .. "$")
      if idx then return saved[tonumber(idx)] end
      return s
    end
  }

  return doc
end
