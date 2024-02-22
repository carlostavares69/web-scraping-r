@echo off
jupyter trust ./DataScraping-SSPDS-CE.ipynb
jupyter notebook --NotebookApp.allow_origin='https://colab.research.google.com/' --port=8888 --NotebookApp.port_retries=0 --NotebookApp.kernel_name=r ./DataScraping-SSPDS-CE.ipynb