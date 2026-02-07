import { WebR } from 'https://webr.r-wasm.org/latest/webr.mjs';

document.addEventListener('DOMContentLoaded', async () => {
  const container = document.getElementById('webr-container');
  if (!container) return;

  const statusEl = container.querySelector('.webr-status');
  const editorEl = container.querySelector('.webr-editor');
  const outputEl = container.querySelector('.webr-output');
  const runBtn = container.querySelector('.webr-run-btn');
  const fallback = container.querySelector('.webr-fallback');

  if (fallback) fallback.style.display = 'none';
  if (statusEl) statusEl.style.display = 'block';
  if (editorEl) editorEl.style.display = 'block';
  if (outputEl) outputEl.style.display = 'block';
  if (runBtn) runBtn.style.display = 'inline-block';

  const updateStatus = (msg, loading = true) => {
    if (statusEl) {
      statusEl.innerHTML = loading
        ? '<span class="spinner-border spinner-border-sm me-2"></span>' + msg
        : msg;
    }
  };

  updateStatus('Initializing WebR...');

  try {
    const webR = new WebR();
    await webR.init();

    updateStatus('Installing dplyr (this may take a moment)...');
    await webR.installPackages(['dplyr'], { quiet: true });

    updateStatus('Loading dplyr...');
    await webR.evalRVoid('library(dplyr)');

    updateStatus('Ready! Edit the code and click "Run Code" to execute.', false);

    runBtn.disabled = false;
    runBtn.addEventListener('click', async () => {
      const code = editorEl.value;
      outputEl.textContent = 'Running...';
      runBtn.disabled = true;

      try {
        await webR.objs.globalEnv.bind('.webr_code', code);
        await webR.evalRVoid(`
          .webr_output <- capture.output({
            .webr_result <- tryCatch(
              withVisible(eval(parse(text = .webr_code))),
              error = function(e) list(value = paste("Error:", e$message), visible = TRUE)
            )
            if (.webr_result$visible) print(.webr_result$value)
          }, type = "output")
        `);

        const result = await webR.evalR('.webr_output');
        const output = await result.toArray();
        outputEl.textContent = output.join('\n') || '(No output)';
      } catch (e) {
        outputEl.textContent = 'Error: ' + e.message;
      } finally {
        runBtn.disabled = false;
      }
    });

  } catch (e) {
    updateStatus('Failed to initialize WebR: ' + e.message, false);
    console.error('WebR initialization failed:', e);
  }
});
