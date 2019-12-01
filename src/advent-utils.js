export function mountSolution(application, solutionNumber, solutionInput) {
  const answer = document.createElement("div");

  const container = document.createElement("div");
  container.style.marginBottom = "16px";
  container.innerHTML = `<b>(Solution ${solutionNumber})</b>`;

  container.appendChild(answer);
  document.body.appendChild(container);

  fetch(solutionInput)
    .then(res => res.text())
    .then(input => {
      application.init({
        node: answer,
        flags: {
          part: solutionNumber,
          input
        }
      });
    });
}
