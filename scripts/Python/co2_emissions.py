# Ce script permet de calculer la consommation électrique d'un script R, ainsi que les émissions en équivalent C02 correspondant.


from codecarbon import EmissionsTracker
import subprocess
import sys

# Chemin complet vers Rscript.exe
rscript_path = "C:/Program Files/R/R-4.4.0/bin/Rscript.exe" 

tracker = EmissionsTracker()
tracker.start()

try:
    # Utiliser le chemin complet vers Rscript.exe
    result = subprocess.run([rscript_path, "C:/Users/gmace/PAB/Projet Bois-buche/scripts/R/brouillon.R"], check=True, capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr, file=sys.stderr)
except subprocess.CalledProcessError as e:
    print(f"Erreur lors de l'exécution du script R: {e}", file=sys.stderr)

emissions = tracker.stop()
print(f"Emissions de CO2 : {emissions} kg")


