// src/index.js
import { buildIndex, search } from "./search.js";

let index = [];
let allEntries = [];

async function init() {
  const res = await fetch("./data/entries.json");
  if (!res.ok) throw new Error(`Failed to load entries.json (${res.status})`);
  allEntries = await res.json();
  index = buildIndex(allEntries);
  render("");
}

/* Minimal HTML escaping so entry text can't break the markup */
function esc(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/* Category is a single string in the data — render as one badge.
   (If you ever store it as an array, the Array.isArray check handles both.) */
function renderCategory(category) {
  const cats = Array.isArray(category) ? category : [category];
  return cats
    .filter(Boolean)
    .map((c) => `<span class="badge">${esc(c)}</span>`)
    .join(" ");
}

function mdInline(s) {
  return esc(s).replace(/`([^`]+)`/g, "<code>$1</code>");
}

function renderEntry(entry, expandedId) {
  const expanded = entry.id === expandedId;
  const details = expanded
    ? `
      <div class="entry-details">
        <h2 class="entry-title"><code>${esc(entry.title)}</code></h2>
        <p class="entry-category"><i> Dimensions level: ${renderCategory(entry.category)} </i></p>
        <p class="entry-body">${mdInline(entry.body)}</p>
        <pre class="entry-example"><code>${esc(entry.example)}</code></pre>
      </div>`
    : "";

  return `
    <li class="entry ${expanded ? "expanded" : ""}" data-id="${esc(entry.id)}">
      <button class="entry-header" aria-expanded="${expanded}">
        <strong class="entry-title">${esc(entry.title)}</strong>:
        <span class="entry-summary">${esc(entry.summary)}</span>
      </button>
      ${details}
    </li>`;
}

let expandedId = null;

function render(query) {
  const list = document.getElementById("results");
  const hits = search(index, query);

  list.innerHTML = hits.length
    ? hits.map((e) => renderEntry(e, expandedId)).join("")
    : `<li class="no-results">No matches for “${esc(query)}”</li>`;
}

/* Event delegation: one listener on the list handles every entry,
   including entries re-rendered after a search. */
document.getElementById("results").addEventListener("click", (ev) => {
  const header = ev.target.closest(".entry-header");
  if (!header) return;

  const li = header.closest(".entry");
  const id = li.dataset.id;

  // Toggle: clicking the open entry collapses it; clicking another opens it.
  expandedId = expandedId === id ? null : id;

  // Re-render with the current query so search state is preserved.
  render(document.getElementById("q").value);
});

const input = document.getElementById("q");
let timer;
input.addEventListener("input", (ev) => {
  clearTimeout(timer);
  timer = setTimeout(() => {
    expandedId = null; // collapse any open entry on new search
    render(ev.target.value);
  }, 150);
});

init();