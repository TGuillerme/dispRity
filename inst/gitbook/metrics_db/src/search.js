const normalize = (s) =>
  String(s ?? "")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")   // strip accents
    .replace(/[^\w\s-]/g, " ")         // strip punctuation
    .replace(/\s+/g, " ")
    .trim();

export function buildIndex(entries) {
  return entries.map((entry) => ({
    ref: entry,
    haystack: normalize(
      [
        entry.title,
        entry.category,
        entry.summary,
        entry.body,
        ...(entry.tags || []),
        ...(entry.keywords || []),
      ].join(" ")
    ),
    titleNorm: normalize(entry.title),
  }));
}

export function search(index, query, { limit = 50 } = {}) {
  const tokens = normalize(query).split(" ").filter(Boolean);
  if (!tokens.length) return index.map((r) => r.ref).slice(0, limit);

  const scored = [];

  for (const row of index) {
    let score = 0;
    let matchedAll = true;

    for (const token of tokens) {
      const inTitle = row.titleNorm.includes(token);
      const inBody = row.haystack.includes(token);

      if (!inBody) { matchedAll = false; break; }
      score += inTitle ? 3 : 1;          // title hits weigh more
    }

    if (matchedAll) scored.push({ entry: row.ref, score });
  }

  return scored
    .sort((a, b) => b.score - a.score)
    .slice(0, limit)
    .map((s) => s.entry);
}