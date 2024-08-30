! pip install git+https://github.com/openai/whisper.git
! pip install jiwer
import whisper

# On définit le chemin de l'enregistrement de son entretien sur son PC
entretien = "C:\\Users\\gmace\\PAB\\Projet Bois-buche\\enregistrement_rdv1_chatelin.mp3" 
# Chemin du fichier audio à transcrire

modele_whisper = "large-v2"
print("Chargement du modèle")
model = whisper.load_model(modele_whisper)
 
# Transcription
print("Transcription commencée")
 
transcription = model.transcribe(entretien)
 
print("Transcription terminée")