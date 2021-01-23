const timer = "timer";

const startFromZero = (val) => {
  return +val > 9 ? `${val}` : `0${val}`;
};

const setCurrentTime = (timerEl, dateEl) => {
  const now = new Date();
  timerEl.textContent = `
 ${startFromZero(now.getHours())}:${startFromZero(now.getMinutes())}
  `;
  dateEl.textContent = `
  ${startFromZero(now.getDate())}.${startFromZero(
    now.getMonth() + 1
  )}.${now.getFullYear()}
`;
};

export const registerTimeTicker = (timerEl, dateEl) => {
  setInterval(() => setCurrentTime(timerEl, dateEl), 1000);
};
