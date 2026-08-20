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

  // Keep documentation accordions usable even when the Bootstrap CDN script is
  // unavailable (for example, in a local preview behind a restricted network).
  document.querySelectorAll(".docs-content .accordion").forEach((accordion) => {
    const buttons = [...accordion.querySelectorAll(".accordion-button")];
    buttons.forEach((button) => {
      button.addEventListener("click", () => {
        const panel = document.getElementById(button.getAttribute("aria-controls"));
        if (!panel) return;
        const willOpen = !panel.classList.contains("show");
        buttons.forEach((otherButton) => {
          const otherPanel = document.getElementById(otherButton.getAttribute("aria-controls"));
          otherButton.classList.add("collapsed");
          otherButton.setAttribute("aria-expanded", "false");
          otherPanel?.classList.remove("show");
        });
        if (willOpen) {
          button.classList.remove("collapsed");
          button.setAttribute("aria-expanded", "true");
          panel.classList.add("show");
        }
      });
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
