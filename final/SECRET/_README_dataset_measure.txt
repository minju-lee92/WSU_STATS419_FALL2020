The file "measure-students.txt" contains the dataset.  

This needs to be a SECRET, meaning it DOES NOT go on github.

You include it from a local path, and provide instructions in the "Final Write" regarding the importance of keeping it secret, and include it in a generic form.

paste0(local.path, "measure-students.txt");  

Your "data_collector" identifier has been passed through the "md5" algorithm one time.  So you can still identify your data.

The data quality is unknown.  You know the quality of your collection process, but cannot assume the quality of your peers is better or worse.  Ultimately, data integrity is linked to student integrity, and you should discuss this as part of your writeup.

I will make a RMarkdown template for your final writeup next.