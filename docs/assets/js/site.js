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

  const copyText = async (text) => {
    if (navigator.clipboard?.writeText) {
      await navigator.clipboard.writeText(text);
      return;
    }
    const input = document.createElement("textarea");
    input.value = text;
    input.setAttribute("readonly", "");
    input.style.position = "fixed";
    input.style.opacity = "0";
    document.body.append(input);
    input.select();
    document.execCommand("copy");
    input.remove();
  };

  document.querySelectorAll(".docs-content pre > code").forEach((code) => {
    const block = code.parentElement;
    block.classList.add("code-block");
    const button = document.createElement("button");
    button.type = "button";
    button.className = "code-copy";
    button.setAttribute("aria-label", "Copy code to clipboard");
    button.innerHTML = '<i class="bi bi-clipboard me-1" aria-hidden="true"></i>Copy';
    button.addEventListener("click", async () => {
      try {
        await copyText(code.textContent);
        button.innerHTML = '<i class="bi bi-check2 me-1" aria-hidden="true"></i>Copied';
        button.classList.add("copied");
      } catch {
        button.textContent = "Copy failed";
      }
      window.setTimeout(() => {
        button.innerHTML = '<i class="bi bi-clipboard me-1" aria-hidden="true"></i>Copy';
        button.classList.remove("copied");
      }, 1800);
    });
    block.prepend(button);
  });
})();
