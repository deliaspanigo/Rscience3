# 1. Preparar y Guardar
git add .
git commit -m "Release: Versión 0.5.2 con mejoras generales"

# 2. Etiquetar la versión
# Cambié '0.5.w' por 'v0.5.2' para que sea estándar
git tag -a v0.5.2 -m "Versión estable 0.5.2 del paquete"

# 3. Subir TODO (cambios y etiquetas) de un solo golpe
git push origin main --follow-tags# 1. Preparar y Guardar
git add .
git commit -m "Release: Versión 0.5.2 con mejoras generales"

# 2. Etiquetar la versión
# Cambié '0.5.w' por 'v0.5.2' para que sea estándar
git tag -a v0.5.2 -m "Versión estable 0.5.2 del paquete"

# 3. Subir TODO (cambios y etiquetas) de un solo golpe
git push origin main --follow-tags
