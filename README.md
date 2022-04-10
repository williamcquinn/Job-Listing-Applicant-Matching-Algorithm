# Job-Resume-Matching-ML-Algorithm
Resumatch: Built a resume to job matching algorithm, that output optimal jobs tailored to your resume depending on desired location, desired industry, and key words. Created an easy-to-use interface with R Shiny where the user could upload their resume, visualize the number of positions in the desired U.S. region, and receive match scores for optimal job titles as well as feedback on recommended keywords to include in the resume to increase their visibility in the application process for highly scored job titles. 

The job search is a daunting task, there are a variety of employment websites, that once you have uploaded your resume, share a long list of jobs that the user must dig through to find something even close to compatible. In personal experience, we have struggled with determining the legitimacy of these matches and we all have experience with the sponsored emails from sites like ZipRecruiter and Monster that seem more like a shot in the dark than a realistic recommendation to apply to.

Even if you are not actively looking for a job, just knowing where your resume stands in relation to what companies are looking for in a job candidate can help you prepare for when you start searching. Recent graduates like us need all the information we can to effectively plan our next step, having a clear picture about the state of the industry in terms of the geographic location of job offerings can help us plan for possible relocation in the near future.

The goal of this project was to create an easy to use interface where the user can upload their resume, visualize the number of positions in U.S. regions related to the data science industry, and receive the top sectors, industries, and job listings that match their skills and experience written in their resume.

Not only that, this product also offers the user descriptive feedback on which keywords to include in their resume, which would more closely match their resumes to the job descriptions of their desired positions in their application process.

**Data**

Three datasets from Glassdoor and one dataset from Indeed found on Kaggle were used to demonstrate technical viability. With more time a webscraping script would have been employed to access live positions on Indeed. The processing data was composed of 10,000+ jobs and relevant variables were "Job Title", "Job Description", "Location", and "Sector". Stop words and common words were excluded and the data was grouped by job title, location, and sector.

**Word Count**

Job descriptions were broken down into keywords (single words and bi-grams). Dictionaries were created for sector level, title level, and region level, identifying the top 100 key words for each level, such as 'python', 'machine', 'spark', 'big data', 'machine learning', 'processing', 'sql',
'tensorflow', and 'aws'.

**Word Match**

An input resume was read using the library 'pdftools' and converted to text then keywords. Key words were matched to the appropriate dictionaries to return a table of match scores. For example, my input resume had these series of Title Matches: "Business/Data Analyst 72.0%", "Data Scientist 58%", "Data Engineer 56%", and "Business Developer 48.0%". 

**Keywords Feedback**

Probably the most lucrative component to the Resumatch dashboard was that after successfully matching a resume to job titles, the algorithm output recommended keywords to consider adding to the users resume using the robust keywords dictionary. Keywords recommended were based on improving the users overall matching score for the series of optimal job titles. For example, recommended keywords for "Machine Learning Engineer" could be "DevOps", "ML pipelines", "MongoDB", "hadoop", and so on.  

**Visualization**

The location of jobs were sorted according to regions (Mountain, West, Midwest, Central, North East, South East). An interactive map of the U.S displayed the number of jobs in a region allowing the user to click on a point and see information about a suggested role, using the Maps and Leaflet libraries.

**Use Case**

This product would be most advantageous to younger professionals in the IT/Data industry on the job market, but could also be useful for employers looking for best-fit employees and users who are looking to improve their resumes to better align with a desired title in a desired region.

**Potential Upgrades**

• Supporting more filetypes, although you should try to always submit your resume in pdf format

• Continuously expanding and updating job listing data

• Expanding the job listing data to the global market

• Recommendations on how to best improve the users skillset
