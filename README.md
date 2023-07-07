# An audit log for ODK Central edits via ruODK

In my work with research and clinical trials, I am often asked how we can make an audit log of all changes made to a submission either during the initial data collection on ODK Collect/Enketo (this is done with the XLSForm audit functions) and during the post-submission editing phase.

The goal is basically to be able to download a list of edits made on ODK Central. 
Key information which is required is

* Which submission was changed?
* What was changed [from > to]?
* When was it changed?
* Who changed it?
* Why did they change it?

Central provides a really nice visualisation of the change-log, via the ‘more’ button when you hover over an entry’s management buttons. The problem is that there’s no obvious or easy way to download a complete copy of this information in a way that’s compatible with the five questions above. 

The purpose of this showcase is to demonstrate how we can use existing API calls to get all the info we need in to a single table. Note that I’m using ruODK because that’s my favourite language, but I am sure someone could do a python version pretty easily. 

ruODK doesn’t currently (v1.4.0) have endpoints for a couple of the API calls that are needed for this work. I’ve done a pretty horrible job of hacking @Florian_May’s code and created some new ruODK-derived functions that do what I want; specifically to get the list of differences and the list of comments about edits. 


Attached below are an R script and example output of the form. 

[Archive.zip|attachment](upload://9baSr2I9U6nduT8V8xXXl4W1gva.zip) (4.2 KB)

The R script is fully annotated and so should be reasonably easy to understand. 

The report looks like this >>>
![Screenshot 2023-07-07 at 08.58.03|690x258](upload://jDOakVwbQSKX2rziFDI2Td6Osqn.jpeg)



Thanks a million billion to @ktuite and @Florian_May for their expert advice on the API and ruODK.
I’d appreciate comments and suggestions 

I think we still need to figure out how to add Approvals/Rejections of submissions and comments about that, but that should be relatively simple to do. 

It seems likely that entitites will add a new layer of complexity in terms of how many tables we need to make, but the core concept is (I think) all here and all working.
