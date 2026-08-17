(() => {
  const currentPath = window.location.pathname.replace(/index\.html$/, "").replace(/\/$/, "");
  const links = [...document.querySelectorAll(".docs-sidebar .nav-link")];

  links.forEach((link) => {
    const linkPath = new URL(link.href).pathname.replace(/index\.html$/, "").replace(/\/$/, "");
    if (linkPath === currentPath) link.classList.add("active");
  });

  const filter = document.querySelector("#nav-filter");
  filter?.addEventListener("input", () => {
    const query = filter.value.trim().toLowerCase();
    links.forEach((link) => {
      const text = `${link.textContent} ${link.dataset.navSearch || ""}`.toLowerCase();
      link.closest("a").hidden = query.length > 0 && !text.includes(query);
    });
  });

  document.querySelectorAll(".docs-sidebar .nav-link").forEach((link) => {
    link.addEventListener("click", () => {
      bootstrap.Offcanvas.getInstance(document.querySelector("#docsNavigation"))?.hide();
    });
  });
})();
