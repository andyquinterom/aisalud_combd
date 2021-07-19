# Analítica Integrada Salud - ComBD

ComBD es una aplicación para administrar y manipular los datos para Analítica
Integrada Salud.

# Cómo correr la aplicación

> Para correr la aplicación se debe tener una base de datos de
> [PostgreSQL](https://www.postgresql.org/) corriendo.

## En computador local

Para correr la aplicación localmente se debe clonar el repositorio. En un
terminal con git instalado se debe hacer 
`git clone https://github.com/FundacionAIS/aisalud_combd`.

Dentro del directorio aisalud se debe crear un nuevo archivo llamado `.Renviron`
con los credenciales de la base de datos.

```
# Nombre de la base de datos
DATABASE_NAME = ...
# Nombre de usuario
DATABASE_USER = ...
# Contraseña del usuario
DATABASE_PW = ...
# Dirección IP de la base de datos
DATABASE_HOST = ...
# Puerto de la base de datos
DATABASE_PORT = 5432
# Almacenamiento de la base de datos en MB
DATABASE_MAX_STORAGE = 1024
```

Para instalar las dependencias se debe entrar a R en el directorio e instalar
el paquete [renv](https://github.com/rstudio/renv). Después se ejecuta
`renv::restore()` para instalar las dependencias.

```bash
# Algunos de los paquetes que utiliza Analítica Integrada Salud dependen
# de bibliotecas externas. En sistemas operativos basados en Debian Linux
# estos se pueden instalar con el siguiente commando:

apt-get update && apt-get install -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    git-all \
    libv8-dev \
    libpq-dev \
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libproj-dev \
    proj-data \
    proj-bin \
    libgeos-dev
 ```

Finalmente se ejecuta el comando `shiny::runApp()` para correr la aplicación.

# Contribuir 

Para contribuir al proyecto lee el lineamiento de contribuidores
(**CONTRIBUTING.md**) y pide ayuda en la sección de discusiones. Siempre
buscamos personas dispuestas a contribuir y ayudar a mejorar
A
