# flymake as unified syntax and grammar checker backend

The Doom `syntax` module is loaded with `+flymake`, making flymake the checker backend for eglot and vale. `flycheck-grammarly` was removed because it introduced flycheck as a second, competing error-display system. Grammar checking is provided by `flymake-languagetool` using the local LanguageTool jar at `/opt/LanguageTool-5.5/languagetool-server.jar`. `ltex-ls` (eglot-ltex) was considered but rejected because the project has been archived.
