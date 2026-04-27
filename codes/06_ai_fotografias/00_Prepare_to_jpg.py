import os
import fitz  # PyMuPDF
from PIL import Image
from tqdm import tqdm

def list_files_in_directory(directory):
    file_paths = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_path = os.path.join(root, file)
            file_paths.append(file_path)
    return file_paths

def convert_pdf_to_images(pdf_path):
    print(f"Converting PDF to images: {pdf_path}")
    pdf_document = fitz.open(pdf_path)
    output_dir = os.path.dirname(pdf_path)  # Obtener el directorio del archivo PDF
    for page_num in range(len(pdf_document)):
        page = pdf_document.load_page(page_num)
        pix = page.get_pixmap()
        img = Image.frombytes("RGB", [pix.width, pix.height], pix.samples)
        output_path = os.path.join(output_dir, f"{os.path.basename(pdf_path)}_page_{page_num + 1}.jpg")
        img.save(output_path, "JPEG")
        print(f"Saved image: {output_path}")

if __name__ == "__main__":
    directory = 'org'  # Cambia esto a tu directorio de interés
    
    file_paths = list_files_in_directory(directory)
    print(f"Found {len(file_paths)} files in directory {directory}")
    
    # Convertir PDFs a imágenes con barra de progreso
    for file_path in tqdm(file_paths, desc="Processing files"):
        if file_path.lower().endswith('.pdf'):
            convert_pdf_to_images(file_path)