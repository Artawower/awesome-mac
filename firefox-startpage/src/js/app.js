import { registerTimeTicker } from "./time";

const getElementById = (document, id) => {
  return document.getElementById(id);
};

const addClickListener = (htmlEl) => (func) => {
  return htmlEl.addEventListener("click", func);
};

const addInputListener = (htmlEl) => (func) => {
  return htmlEl.addEventListener("input", (e) => {
    func(htmlEl.value);
  });
};

const addKeypressListener = (htmlEl) => (func) => {
  return htmlEl.addEventListener("keypress", (e) => {
    func(e);
  });
};

const addReturnPressListener = (htmlEl, func) => {
  htmlEl.addEventListener("keypress", (e) => {
    if (e.key !== "Enter") {
      return;
    }
    func();
  });
};

const setActivePane = (panes, paneName) => {
  panes.forEach((p) => {
    if (p.name == paneName) {
      p.btn.classList.add("active");
      p.pane.classList.add("show");
      return;
    }
    p.btn.classList.remove("active");
    p.pane.classList.remove("show");
  });
};

const collectPanes = (document, paneNames) => {
  return paneNames.map((n) => ({
    name: n,
    pane: getElementById(document, n + "Pane"),
    btn: getElementById(document, n + "Btn"),
  }));
};

const registerPaneSwitcher = (panes) => {
  panes.forEach((p) =>
    addClickListener(p.btn)(() => setActivePane(panes, p.name))
  );
};

const registerSearchExecution = (searchInput) => {
  addReturnPressListener(searchInput, () => {
    window.open(`https://google.com/search?q=${searchInput.value}`, "_blank");
  });
};

const registerSearchSuggestions = (searchInput) => {
  // useless now
  // const sendQuery = (query) => {
  //   console.log(query);
  //   if (query.length < 3) {
  //     return;
  //   }
  //   const xhr = new XMLHttpRequest();
  //   xhr.open(
  //     "GET",
  //     `https://www.google.com/complete/search?q=${encodeURIComponent(query)}`,
  //     true
  //   );
  //   xhr.setRequestHeader("Accept", "*/*");
  //   xhr.setRequestHeader("Access-Control-Allow-Origin", "*");
  //   // xhr.setRequestHeader("Content-type", "application/json");
  //   xhr.onreadystatechange = function () {
  //     if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
  //       console.log(xhr.response);
  //     }
  //   };
  //   xhr.send();
  // };
  // addInputListener(searchInput)(sendQuery);
};

const createFlyLetter = (document) => {
  return (e) => {
    const audio = new Audio("sounds/click2.mp3");
    audio.play();
    const startPage = getElementById(document, "startPage");
    const letter = document.createElement("div");
    letter.className = "fly-letter";
    letter.textContent = e.key;
    letter.style.left = `${Math.random() * 80 + 10}%`;

    startPage.appendChild(letter);

    setTimeout(() => {
      letter.remove();
    }, 2020);
  };
};

const registerSearchAnimation = (document, searchInput) => {
  addKeypressListener(searchInput)(createFlyLetter(document));
};

const registerSearchInput = (window) => {
  const searchInput = getElementById(window.document, "searchInput");
  registerSearchExecution(searchInput);
  registerSearchAnimation(window.document, searchInput);
  registerSearchSuggestions(searchInput);
};

const toggleFooter = (footerEl, showBtnEl) => {
  if (footerEl.classList.contains("show")) {
    footerEl.classList.remove("show");
    showBtnEl.classList.remove("opened");
    return;
  }
  footerEl.classList.add("show");
  showBtnEl.classList.add("opened");
};
const typingLatency = 250;
const titleResetingLatency = 5000;

const registerFooterShower = (document) => {
  const showPane = getElementById(document, "show-pane");
  const footerEl = getElementById(document, "footer");
  addClickListener(showPane)(() => toggleFooter(footerEl, showPane));
};

const addCarriage = (el) => {
  const carriage = document.createElement("span");
  carriage.classList.add("carriage");
  carriage.textContent = "|";
  el.appendChild(carriage);
};

const writeRandomTitle = (titleEl) => {
  const msgs = [
    "Hello Artur.",
    "How are you?",
    "Be happy",
    "Do you best!",
    "You're not alone",
  ];
  const randomIndex = Math.floor(Math.random() * msgs.length);
  const msg = msgs[randomIndex];
  console.log(msg, randomIndex);
  writeTitle(titleEl, msg);
};

const deleteTitle = (titleEl) => {
  const deleter = () =>
    setTimeout(() => {
      if (titleEl.textContent === "> |") {
        writeRandomTitle(titleEl);
        return;
      }
      titleEl.textContent = titleEl.textContent.slice(
        0,
        titleEl.textContent.length - 2
      );
      addCarriage(titleEl);
      deleter();
    }, typingLatency);
  deleter();
};

const writeTitle = (titleEl, title) => {
  let i = 0;

  const writer = () =>
    setTimeout(() => {
      titleEl.textContent = "> " + title.slice(0, i);
      addCarriage(titleEl);
      if (i >= title.length) {
        setTimeout(() => deleteTitle(titleEl), titleResetingLatency);
        return;
      }
      i++;
      writer();
    }, typingLatency);

  writer();
};

const registerHeaderTyping = (document) => {
  const title = getElementById(document, "cli");
  writeTitle(title, "Hello Artur.");
};

document.addEventListener("DOMContentLoaded", () => {
  const paneNames = ["search", "widgets", "translate"];
  const panes = collectPanes(document, paneNames);
  // registerPaneSwitcher(panes);
  registerSearchInput(window);
  registerTimeTicker(
    getElementById(document, "timer"),
    getElementById(document, "date")
  );
  registerFooterShower(document);
  registerHeaderTyping(document);
});
