import os
import pandas as pd
import re
import base64
from PIL import Image
from io import BytesIO
from dotenv import load_dotenv
import openai
from tqdm import tqdm

# Cargar las variables de entorno desde el archivo .env
load_dotenv()

# Configurar la clave de API de OpenAI
openai.api_key = os.getenv("OPENAI_API_KEY")

def list_files_in_directory(directory):
    file_paths = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_path = os.path.join(root, file)
            file_paths.append(file_path)
    return file_paths

def create_excel_from_paths(file_paths, output_file):
    df = pd.DataFrame(file_paths, columns=['File Path'])
    
    # Clasificar imágenes y añadir columnas con la clasificación y evaluación
    df[['Classification', 'Description', 'Assessment', 'Rating']] = df.apply(lambda row: classify_image(row['File Path']), axis=1, result_type='expand')
    
    df.to_excel(output_file, index=False)

def classify_image(image_path):
    print(f"Processing image: {image_path}")
    
    try:
        with open(image_path, "rb") as image_file:
            img = Image.open(image_file)
            img = img.convert("RGB")
            img = img.resize((256, 256))  # Redimensionar la imagen para reducir el tamaño
            buffered = BytesIO()
            img.save(buffered, format="JPEG", quality=50)  # Reducir la calidad de la imagen
            img_b64_str = base64.b64encode(buffered.getvalue()).decode('utf-8')
            img_type = "image/jpeg"
    except Exception as e:
        print(f"Error opening image {image_path}: {e}")
        return "Unknown", "N/A", "N/A", "N/A"

    # Define the prompt to process the image
    prompt = f"""
            ### **Prompt for Evaluating Household Conditions**

            You are a **civil engineering or architecture expert** specializing in **assessing vulnerable households**. Your task is to **analyze images of kitchens, bathrooms, floors, and house exteriors** to evaluate their **quality and potential risks** to residents' well-being.

            #### **Step 1: Identify the Type of Space**  
            Determine whether the image depicts a:  
            - **Kitchen**  
            - **Bathroom**  
            - **Floor**  
            - **House Exterior**  

            #### **Step 2: Provide a Detailed Description**  
            Evaluate the following aspects:  
            1. **Materials & Construction**  
            - Describe the **flooring, walls, ceiling, plumbing, and other visible elements**.  
            - Identify the **presence of durable or fragile materials** (e.g., concrete, tiles, wood, metal sheets, dirt).  

            2. **Structural Integrity & Safety**  
            - Identify **cracks, leaks, missing components, or other visible damages**.  
            - Assess **stability, durability, and potential hazards**.  

            3. **Functionality & Livability**  
            - Determine if the space **serves its intended purpose effectively**.  
            - Note **issues such as lack of proper drainage, inadequate lighting, or faulty plumbing**.  

            4. **Sanitary & Health Conditions**  
            - Identify **mold, water accumulation, poor ventilation, exposure to contaminants, or unhygienic conditions**.  
            - Assess **risks to health and well-being**.  

            #### **Step 3: Assign a Quality Rating (0 to 100)**  
            - **0-25 (Very Poor - Urgent Intervention Needed)**  
            - **Severe risks**: Lack of essential infrastructure, hazardous conditions (e.g., dirt floors, exposed wiring, structural instability).  

            - **26-50 (Poor - Requires Significant Improvements)**  
            - **Functional but unsafe**: Basic structure exists but with major deficiencies (e.g., leaks, broken fixtures, inadequate sanitation).  

            - **51-75 (Fair - Functional but Needs Upgrades)**  
            - **Limited but safe**: The space is operational, but conditions could be improved (e.g., worn-out materials, poor ventilation).  

            - **76-100 (Good - Safe & Adequate)**  
            - **Well-built and livable**: Proper construction, **no major deficiencies**, and safe for daily use.  

            #### **Response Format:**  
            - **Type of Space:** *(Kitchen/Bathroom/Floor/Exterior)*  
            - **Description:** *(Materials, structural condition, functionality, sanitary concerns)*  
            - **Assessment:** *(Very Poor / Poor / Fair / Good)*  
            - **Quality Rating (0-100):** *(Numerical score based on condition and risks)*  
    """

    try:
        response = openai.ChatCompletion.create(
            model="gpt-4o-mini",
            messages=[
                {
                    "role": "user",
                    "content": [
                        {"type": "text", "text": prompt},
                        {
                            "type": "image_url",
                            "image_url": {"url": f"data:{img_type};base64,{img_b64_str}"},
                        },
                    ],
                }
            ],
            max_tokens=250
        )

        response_content = response.choices[0].message['content'].strip()
        
        # Imprimir la salida completa de OpenAI
        print(f"OpenAI response for image {image_path}: {response_content}")
    
        # Extraer la clasificación y evaluación de la respuesta
        lines = response_content.split('\n')
        type_line = next((line for line in lines if line.startswith('- **Type of Space:**')), 'Type: Unknown')
        description_start = next((i for i, line in enumerate(lines) if line.startswith('- **Description:**')), None)
        assessment_line = next((line for line in lines if line.startswith('- **Assessment:**')), 'Assessment: N/A')
        rating_line = next((line for line in lines if line.startswith('- **Quality Rating (0-100):**')), 'Rating: N/A')
        
        classification = type_line.split('- **Type of Space:** ')[1].strip() if '- **Type of Space:** ' in type_line else 'Unknown'
        description = '\n'.join(lines[description_start:lines.index(assessment_line)]).strip() if description_start is not None else 'N/A'
        assessment = assessment_line.split('- **Assessment:** ')[1].strip() if '- **Assessment:** ' in assessment_line else 'N/A'
        rating = rating_line.split('- **Quality Rating (0-100):** ')[1].strip() if '- **Quality Rating (0-100):** ' in rating_line else 'N/A'
        
        print(f"Classification: {classification}")
        print(f"Description: {description}")
        print(f"Assessment: {assessment}")
        print(f"Rating: {rating}")
        
        return classification, description, assessment, rating

    except Exception as e:
        print(f"Error processing image {image_path}: {e}")
        return "Unknown", "N/A", "N/A", "N/A"

if __name__ == "__main__":
    directory = 'org'  # Cambia esto a tu directorio de interés
    output_file = 'lista_fotografias_piloto.xlsx'
    file_paths = list_files_in_directory(directory)
    
    # Crear un DataFrame vacío para almacenar los resultados
    results = []

    # Procesar las imágenes con barra de progreso
    for file_path in tqdm(file_paths, desc="Processing files"):
        classification, description, assessment, rating = classify_image(file_path)
        results.append({
            'File Path': file_path,
            'Classification': classification,
            'Description': description,
            'Assessment': assessment,
            'Rating': rating
        })
    
    # Crear el DataFrame final y guardarlo en un archivo Excel
    df = pd.DataFrame(results)
    df.to_excel(output_file, index=False)