FROM node:20.6.0

# Install R
RUN apt-get update && apt -y install r-base

RUN Rscript -e 'install.packages("sf", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("dplyr", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("tidyr", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("data.table", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("geojsonsf", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("jsonlite", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("showtext", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'install.packages("curl", repos = "http://cran.us.r-project.org")'

# Install the app.
RUN mkdir -p /usr/app 
WORKDIR /usr/app

COPY package.json .
COPY package-lock.json .

RUN npm ci

COPY . .

RUN npm run build

CMD ["npm", "run", "start"]
