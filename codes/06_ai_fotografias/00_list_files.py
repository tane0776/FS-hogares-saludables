 import os
import pandas as pd
import re
import base64
from dotenv import load_dotenv
import openai

load_dotenv()

openai.api_key = os.getenv("OPENAI_API_KEY")

def list_files_in_directory(directory, limit=10):
    file_paths = []
    folder_count = 0
    for root, dirs, files in os.walk(directory):
        if folder_count >= limit:
            break
        for file in files:
            file_path = os.path.join(root, file)
            file_paths.append(file_path)
        folder_count += 1
    return file_paths

def create_excel_from_paths(file_paths, output_file):
    df = pd.DataFrame(file_paths, columns=['File Path'])
    
    # Clasificar imágenes y añadir columnas con la clasificación y evaluación
    df[['Classification', 'Description', 'Assessment']] = df.apply(lambda row: classify_image(row['File Path']), axis=1, result_type='expand')
    
    df.to_excel(output_file, index=False)

def classify_image(image_path):
    print(f"Processing image: {image_path}")
    
    with open(image_path, "rb") as image_file:
        img_data = image_file.read()
        img_b64_str = base64.b64encode(img_data).decode('utf-8')
        img_type = "image/jpeg"  # Cambia esto si tus imágenes no son JPEG

    # Define the prompt to process the image
    prompt = """
    You are a civil engineering or architecture expert specializing in evaluating vulnerable households. Your task is to assess images of kitchens, bathrooms, floors, and house exteriors to determine their quality and potential risks to residents' well-being.

    Analyze the following image:

    **Step 1: Identify the Type of Space**  
    - Determine if the image is of a **kitchen, bathroom, floor, or house exterior**.  
    
    **Step 2: Provide a Short Description**  
    - **Materials Used:** Describe flooring, walls, ceiling, plumbing, and other visible elements.  
    - **Structural Integrity:** Identify cracks, leaks, missing parts, or visible damage.  
    - **Functionality:** Assess if the space is operational for its intended purpose.  
    - **Sanitary & Health Conditions:** Note any mold, poor ventilation, standing water, or potential hazards.  

    **Step 3: Quality Assessment**  
    - **Very Poor Condition (Urgent Intervention Needed):** Lacks basic infrastructure, severe risks (e.g., dirt floors, exposed pipes, makeshift structures).  
    - **Poor Condition (Needs Improvement):** Existing structure but with significant deficiencies (e.g., broken fixtures, inadequate sanitation, leaks).  
    - **Fair Condition (Functional but Limited):** Basic but safe (e.g., concrete floors, functional drainage but poor ventilation).  
    - **Good Condition (Safe & Adequate):** Proper construction, no major deficiencies, safe for daily use.  

    Provide a structured response with:  
    - **Type:** (Kitchen/Bathroom/Floor/Exterior)  
    - **Description:** (Materials, structure, condition)  
    - **Assessment:** (Very Poor/Poor/Fair/Good)  
    """

    try:
        response = openai.ChatCompletion.create(
            model="gpt-4",
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
    
        # Extraer la clasificación y evaluación de la respuesta
        lines = response_content.split('\n')
        type_line = next((line for line in lines if line.startswith('Type:')), 'Type: Unknown')
        description_line = next((line for line in lines if line.startswith('Description:')), 'Description: N/A')
        assessment_line = next((line for line in lines if line.startswith('Assessment:')), 'Assessment: N/A')
        
        classification = type_line.split(': ')[1] if ': ' in type_line else 'Unknown'
        description = description_line.split(': ')[1] if ': ' in description_line else 'N/A'
        assessment = assessment_line.split(': ')[1] if ': ' in assessment_line else 'N/A'
        
        print(f"Image: {image_path}")
        print(f"Classification: {classification}")
        print(f"Description: {description}")
        print(f"Assessment: {assessment}")
        
        return classification, description, assessment

    except Exception as e:
        print(f"Error processing image {image_path}: {e}")
        return "Unknown", "N/A", "N/A"

if __name__ == "__main__":
    directory = 'org'  # Cambia esto a tu directorio de interés
    output_file = 'lista_fotografias_piloto.xlsx'
    file_paths = list_files_in_directory(directory, limit=10)
    create_excel_from_paths(file_paths, output_file)
