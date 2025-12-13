# install.packages("renv")

# Cargar renv
library(renv)

# Inicializar renv en tu proyecto/paquete
renv::init()


# Cuando quieras guardar el estado actual de dependencias
renv::snapshot()

# Restaurar el entorno exacto si trabajo en otro equipo!
# renv::restore()

# Limpiar paquetes no utilizados
# renv::clean() ocasionalmente para eliminar paquetes no utilizados de la biblioteca privada

# Actualiza los paquetes de manera controlada
# renv::update()
#############################################################
