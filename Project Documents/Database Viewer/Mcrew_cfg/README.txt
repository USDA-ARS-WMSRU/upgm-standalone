README: This file gives a short explanation of all the files.

1. identity.dtd:		Defines the structure of identity data for any entity. Identity data is composed of 'id' (integer) and 'code' (character - as 'P' for process, 'G' for Group and 'O' for operate).

2. identity.xml:		Sample XML to show how identityDTD is used (Not of much use	to anyone apart from developer/maintanier). 
 									
3. param_defn.dtd:		Defines the structure of any parameter for any operation, group, process, or crop. param_defn is composed of param name, type and basic	 unit.
										 
4. param_display.dtd:		Defines the structure of data for displaying any parameter. param_display is composed of param name, param display attribute ('E' - Editable, 'V' - Viewable i.e. only viewing not for editing, 'H' - Hidden) and value attribute ('N' - Number, 'S' - String, 'C' - Choice List).

5. param_lang.dtd:		Defines the language dependent data for any parameter for any operation, group, process or crop. param_lang is composed of param name, param prompt, param alternate unit and param chocies. param choices also have a choice value assigned to each one of them. Also, conversion factor is defined which allows to convert the parameter value from basic unit to alternate unit.

6. crop_db.dtd:  Data definition for crop database record files. crop_db is composed of crop name, param name and value.
 
7. crop_name.crop:		Data files for crop database records in XML format. The files is based upon crop_db.dtd. crop_name will be repalced by appropriate crop name i.e. for eg: alafala.crop for a file with parameter values for alfalfa.

8. crop_db.xsl:			Stylesheet file used for browser display of "crop_name.crop" files.

9. crop_defn.dtd:		Defines the structure of basic crop information. crop_defn is composed of only param_defn (3), which in turn is composed of  param name, type and basic unit. As all crops have the same set of parameters, there no other infomation needed.

10. crop_defn.xml:	Defines the various crop parameters. As, this file is based upon crop_defn.dtd (which in turn upon param_defn.dtd), it gives information of various parameter names, types and basic units.

11. crop_lang.dtd:		Defines the language dependent data of crop parameter. crop_lang is composed of pram_lang (5), which in turn is composed of param name, param prompt, alternate units, conversion factor and param choices.

12. crop_lang.xml:		Contains language dependent data for crop parameters
like param prompt text, alternate units, conversion factor and param choices.

13. crop_display.dtd:		Defines the structure of display data for crop parameter values. This DTD is composed of param_display dtd(4).

14. crop_display.xml:		Defines the data for displaying crop paraeters. This XML is based upon crop_display.dtd (13).

15. crop_defn.xsl:		Stylesheet file used for displaying crop data (from crop_defn.xml, crop_lang.xml and crop_display.xml) in a browser.

16. operation_defn.dtd:		Defines a basic operation database record. operation_defn is composed of identity (1), actionname and param_defn (3). 'actionname' specfies the structure of operation/action name i.e. it defines action name as text for storing 'Biomass manipulation'.

17. operation_defn.xml:		Contains definitions of all valid operations (of all of groups, processes, and operate) and their paramters. 

18. operation_display.dtd:		Defines the operation display data. Its composed of param_display dtd (4).

19. operation_display.xml:		Defines which operation and process parameters are displayable, editable, etc. (Based upon param_display dtd (4)).

20. operation_lang.dtd:		Definition for language dependent data for any operation (prompt, choice, etc from param_lang. dtd (5)).

21. operation_lang.xml:		Contains prompt text, alternate units text, and conversion factors for displaying operation, group, and process parameters listed in operation_defn.xml file.

22. operation_defn.xsl:		Stylesheet file used for displaying operation data (from operation_defn.xml, operation_lang.xml and operation_display.xml) in a browser.

23. operation_db.dtd:	Data definition for operation database record files. oper_db is composed of identity (1) and param name, value pairs.

24. operation_name.oprn: 		Data files containing operation database records in XML format. In reality, "oprn_name" is be repalce by actual operation name.

25. operation_db.xsl:		Stylesheet file used for browser display of "operation_name.oper" files.

Note 1: identityDTD, param_xxx.dtd are used in other DTDs. They have no
existence by themselves.

Note 2: *.crop and *.oprn files are in Crop and Operations sub directories respectfully.