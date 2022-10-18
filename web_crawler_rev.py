from nturl2path import url2pathname
from sre_constants import ANY
from bs4 import BeautifulSoup
import requests as r
import time 
import json
import os
import random
import string
import re
from urllib3 import disable_warnings

class filterer:
    """this will filter out the names and assign the content to the speaker"""
    def __init__(self,paragraph,regex=r"\n[\w\.\s]{10,40}\:\s\(\d{2}\:\d{2}\)") -> None:
        self.paragraph = paragraph
        self.n_chars = 0
        self.pattern = regex
        self.sections = self.map_sections()
        
    def find_person_names(self):
        return re.findall(self.pattern,self.paragraph)

    def split_into_sections(self):
        """obtains the text associated with whomever is speaking.Outputs a dictionary with all of the paragraphs spoken  e.g. {person:[par1,par2,par3]}"""
        return re.split(self.pattern,self.paragraph)

    def map_sections(self):
        """"""
        names = ["header"] + self.find_person_names()
        content = self.split_into_sections()
        if len(names) == len(content):
            d = {}
            for n,c in zip(names,content):
                self.n_chars += len(content)
                d[n] = c
            return d
        else:
            print("Paragraph Unable to process! Uneven sections anem mapping")



    # def filter_all_paragraphs(self,keywords = ["Vaccine"],negative_keywords=['production','mandate']):
    #     """filters the paragraphs associated with people and searches for the ones that caontain the relevant keywords"""
    #     return 
class MyScraper:
    """Class that builds a Webscraper object to scroll through all of the pages on the and extract all the text from the given articles. Outputs """
    def __init__(self,url,out_path) -> None:
        self.url = url
        self.out_path = out_path
        self.content = [] ##stores all of the content
        self.relevant_content = [] ##srotes all of the conent that is flagged as relevant
        self.assigned_sections = []  ##assigns the text to the speaker (Not used)

    def obtain_page_text(self,url) -> str:
        """This function allows us to obtain the text from a given webpage"""
        time.sleep(0.1) ##trying to ensure that the server does not get overloaded
        response = r.get(url)
        if response:
            soup = BeautifulSoup(response.text,features="lxml")
            #print(soup.prettify())
            #from requests_html import HTMLSession
            transcription_body = soup.select('div.fl-callout-text')[0] ##select the transcription by css selector id
            return transcription_body.text
        
        return None

    def obtain_page_titles(self,url):
        """this code obtains all the page titles from the rev website and then calls a function to obtain the text from each link """
        time.sleep(0.1)
        response = r.get(url)
        soup = BeautifulSoup(response.text,features="lxml")
        page_title_els = soup.select('div.pa3.f5')

        page_titles,page_links = [],[]
        ##recursively obatin the parent elements until you obtain the article links
        for title_el in page_title_els:
            page_titles.append(title_el.text)
            par = title_el.parent
            t1 = time.perf_counter()
            while par.name != 'a':
                par = par.parent
                t2 = time.perf_counter()
                t_dif = t2 - t1
                ##checking the time to make sure this in fact does run
                if t_dif % 10 == 0:
                    print(f"{round(t_dif,ndigits=2)} Seconds")

            link = par['href']
            page_links.append(link)

        page_lists = [{"title":title,"content":self.obtain_page_text(link)} for title,link in zip(page_titles,page_links)]
        return page_lists

    def write_to_file(self,selected_content) -> None:
        """writes the content to the Scrapers Specified Path"""
        out_str = os.path.join(self.out_path,'rev_content.txt')
            #for item in content:
            ##write the content to a file
        with open(out_str,'w') as json_file:
            json_file.seek(0)
            json_file.truncate()
            json.dump(selected_content,json_file)
        return None
    
    def obtain_rev_content(self,max_pages) -> None:
        """Crawls through the website in question"""
        start_url = self.url
        response = r.get(start_url)
        if response:
            main_soup = BeautifulSoup(response.text,features="lxml")
        else:
            print("Invalid URL")

        #n_pages_el = main_soup.select("a.page-numbers")[-2] ##obtain the total number of pages to scrape
        #n_pages = int(n_pages_el.text)
        n_pages = int(max_pages)
        ##initiate next Page
        next_page = main_soup.select('a.next.page-numbers')
        next_page_link = next_page[0]['href']
        i = 1
        
        while i < n_pages:
            print(f"Obtaining data from page {i} of {n_pages}...")
            page_dict = self.obtain_page_titles(start_url)
            [self.content.append(item) for item in page_dict]
            selected_content = self.content
            self.write_to_file(selected_content) ##writing the content to the file specified    

            #obtain the next page and set to the starting link to continue loop
            next_page = main_soup.select('a.next.page-numbers')
            next_page_link = next_page[0]['href']
            
            start_url = next_page_link
            main_soup = BeautifulSoup(r.get(next_page_link).text,features="lxml") ##update the main soup to the next page
            i+=1

        return None
    
    def obtainSampleWindow(self,item_content,n_windows = 7,window_size=100,display=True):
        """function obtains n sample windows from the text. This functioned to get a sense of the kind of article that is being looked at"""
        ##obtain a word at random
        repeeat_idxs = []
        i = 0 
        item_content = item_content.split(" ")
        while i < n_windows:
            rand_index = random.randint(0,len(item_content))
            rand_word = item_content[rand_index-1]
            if rand_index not in repeeat_idxs:
                std_window = window_size // 2
                left_lim = rand_index - std_window
                right_lim = rand_index + std_window
                
                if left_lim < 0:
                    correction = 0 - left_lim
                    right_lim += correction

                elif right_lim >= len(item_content):
                    correction = len(item_content) - right_lim 
                    left_lim -= correction
                
                window =  item_content[left_lim:right_lim]
                if display:
                    out_str = " ".join((word for word in window))
                    print(f"sample {i} \n Paragraph: \n {out_str}")

                repeeat_idxs.append(rand_index)
                i += 1

    def output_text_to_files(self,ouput_directory,name="REV"):
        """this file outputs each of the files to the Corpus directory. Takes the content once filtered and then outputs it as individual text files to the desired directory"""
        self.assigned_sections
        for idx,item in enumerate(self.relevant_content):
            item
            save_path = ouput_directory
            name_of_file = f"{name}_source_{idx+1}"
            name_regex = r'\n[\w\.\s]{10,40}\:\s\(\d{2}\:\d{2}\)'
            content = item['content']
            content = re.sub(name_regex,"",content)
            completeName = os.path.join(save_path, name_of_file+".txt")
            
            with open(completeName, "w") as file1:
                toFile = content
                file1.write(toFile)
                
        return

    def clear_all_flags(self):
        """allows items to be revisited. Not required for current implementation"""
        for item in self.content:
            #item['relevant_flag'] = ''
            item['visited_flag'] = ''
           
    def clean_rev_content(self):
        """clean the text content to try and isolate the text of the interview/transcript(as oppposed to the timestamps and names of speakers."""
        for idx,el in enumerate(self.relevant_content):
            text_par = el['content']
            filtered = filterer(text_par)
            filtered.map_sections()
            self.assigned_sections.append(filtered.sections)
            
    def manual_filter_rev_content(self,display_content=False,required_documents=25):
        """this function allows us to filter the data files manually by the relevant names"""
        ##obtain the data from the file
        out_str = os.path.join(self.out_path,'rev_content.txt')
        try:
            self.clear_all_flags()
        except KeyError:
            print("Warning, a keyError has occurred")
            pass

        with open(out_str,'r') as read_file:
            self.content = json.load(read_file)
        ##now we can manually inspect thi
        relevant_count = 0
        required_documents = int(required_documents)
        for idx,item in enumerate(self.content):
            if relevant_count >= required_documents:
                break
             
            print(f"\nArticle no.{idx+1} \n {item['title']}")
            input1 = input("is this relevant? (y/n/m) Hit m for text samples:")
            item['visited_flag'] = 'y' ##set the document to visited
            #item['relevant_flag'] = input1
            if input1 == 'y':
                relevant_count +=1
                self.relevant_content.append(item)
            elif input1 == 'm':
                item_content = str(item['content'])
                sample_cont = self.obtainSampleWindow(item_content)
                input2 = input('are the details of the content relevant?: y/n')
                if input2 == 'y':
                    relevant_count += 1
                    #item['relevant_flag'] = input2
                    self.relevant_content.append(item)
            print(f"{relevant_count} relevant articles")
                
        self.write_to_file(self.relevant_content) 
        return self.content

def scrape_files(outpath,corpus_directory,max_pages_to_scrape=100,documents_required=25):
    ##website specific to scrape from 
    url = "https://www.rev.com/blog/transcripts?s=COVID+Vaccine"
    scraper = MyScraper(url,out_path=outpath)
    scraper.obtain_rev_content(max_pages_to_scrape)
    scraper.manual_filter_rev_content(required_documents=documents_required)
    scraper.clean_rev_content()
    scraper.output_text_to_files(corpus_directory)


#outpath = r"C:\Users\xande\OneDrive\UoC\Texts\Assignments\Assignment1"    ##location to store Preliminary data JSON files
#corpus_directory  = r"C:\Users\xande\OneDrive\UoC\Texts\Assignments\Assignment1\Corpus_covid"   ##directory to place the semi-processed corpus files

def main():
    outpath = input("Please enter the full path where you want to store the scraped content before pre-processing: ")
    corpus_directory = input("Please enter the full directory path you would like to store the corpus once filtered: ")
    docs = input("how many documents do you need to obtain ? (Recommended 25): ")
    max_web_pages = input("Please enter the number of pages you would like to scrape from the website (Recommended 30): ")
    scrape_files(outpath,corpus_directory,max_pages_to_scrape=max_web_pages,documents_required=docs)

main()

# def main(outpath):
#     obtain_rev_content(outpath)
#     clean_data = clean_rev_content()
