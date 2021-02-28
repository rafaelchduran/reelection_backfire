# Reelection Backfire
 Effect of Term Limit Reform on Crime, Violence and Decentralization

GitHub repository for Reelection Backfire project.

 ## Structure of the folders
 
 The folders are structured in such a way as to optimize and facilitate version control. The main folder of the project is in Dropbox and it contains the following subfolders:
 1. **reelection_backfire**: This is the brains of the operation, where all the code and latex files should be included. I expand [below](#main).
 2. **Bibliography**: Include all references here --- pdf from papers, books and the like.
 3. **Data_raw**: Include all original/raw databases here; do not change the names of the original files for the sake of reproducibility.
 4. **Data_waste**: All transformations of databases that are not final go here. It serves as intermediate steps to facilitate running code and debugging efficiently. When the paper is done, this folder can be removed.
 5. **Data_out**: Harmonized and final datasets should be placed here. These are the datasets that will be directly used for analysis.
 6. **Codebooks**: If there are codebooks associated with the data, place them in this folder.
 7. **Results**: Note there are two subfolders: *graphs* and *table*, where graphs and tables obtained from the analysis are stored, respectively. 
 8. **Presentations**: If you create new slides to present the work at conferences or seminars, place them here.

Elements 2-8 are the inputs of the project and thus they are synchronized via dropbox as per usual. The first folder corresponds to the GitHub Repository and should not be synchronized via dropbox, mainly because dropbox synchronization can corrupt the repository. I explain below. 
 
<a id='main'></a>
 ## reelection_backfire
 
If you are a first time user of this repository, the first thing you need to do is to clone it into the Dropbox folder. This is done simply by clicking add in the desktop app, select the option clone repository. Select ```rafaelchduran/reelection_backfire``` and in local path just choose the shared dropbox folder. By doing this you will include a folder called "work-trade-autocracy" in dropbox. 

The next and ***very important*** step to follow is to remove synchorinization of this folder via Dropbox ([instructions](https://help.dropbox.com/en-US/files-folders/restore-delete/ignored-files)). Not doing so will corrupt the repository.

This is the usual list of folders in the repository:
 
1. **R-files**: All R-files for the project go here. (Please make sure to numerate your files [e.g., ```1_data_harmonization.R```] insequence and comment your code to facilitate debugging.)
2. **Do-files**: All Do-files/ado-files for the project go here. (Please make sure to numerate your files [e.g., ```2_descriptive_stats.do```] insequence and comment your code to facilitate debugging.)
3. **Python**: All Python files for the project go here. (Please make sure to numerate your files [e.g., ```5_robustness_remove_rural.py```] insequence and comment your code to facilitate debugging.)
4. **Text**: The latex files for the manuscript go here.

The advantage of synchronizing this folder via GitHub and not via Dropbox is that it makes version control extremely easy. This will allow us to track changes in both the code and the manuscript, and debug whenever necessary. There just some easy jargon to get used to, which is really easy to handle ([video](https://www.youtube.com/playlist?list=PLRqwX-V7Uu6ZF9C0YMKuns9sLDzK6zoiV)):

* *Clone*: When we clone a repository into our folder, we are basically dowloading the repository, and it is ready for synchronization.
* *Commit*: When you have made changes and you want to synchronize them, you open GitHub desktop. This will automatically show what files you have changed and then you can commit them (i.e., synchronize them) by clicking "commit to master." You may want to add a descriptive summary and description. The final step is to click "Push to origin" to synchornize the change into GitHub.
* *Fetch* and *pull*: When somebody else has done changes to the files, you can *fetch* and *pull* them to synchronize them to your computer. Thus before starting a work session, selecte the repository, click the button "Fetch origin" in the desktop Github app. To synchronize this changes into your folder, push "pull origin." This will show you the files that someone else has modified, and what changed.
* *Branch*: This is a little bit more complex but extremely useful. You can move from the current branch (*the master*), and make changes over the files when you are not sure about synchronizing something just yet into the main files. This serves the purpose of experimenting with code or the manuscript. If the experiment is succesful it can be *merged* back into the *master*. When you branch, you commit to the *branch* not to the *master*.

**Note:** *Some people may want to use the command prompt to handle their repositories (usually the reason people are apprehensive about learning GitHub), but this is absolutely unnecessary using the desktop app.*

## Communication via GitHub

Finally, one of the BIG advantages of GitHub is that it will allow us to communicate about issues in the project via the "issues" tab, instead of using long and untractable chains of emails. Also the tab "projects" is very useful to create to-do lists and follow their progress. "Insights" can also provide very useful information. 

One way that facilitates using these tools even further is to download the GitHub app into your Phone or Tablet: it will show changes that have been committed but also the issues that have been raised and other information, moreover you will be able to make comments on issues and such using it. 



