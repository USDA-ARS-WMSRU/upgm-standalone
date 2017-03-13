<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- cropDefnDTD -->
<!--
<!ENTITY % paramdefnDTD SYSTEM "paramDefnDTD.dtd">
%paramdefnDTD;
<!ELEMENT cropdefn (paramdefn*)>
-->

<!-- cropLangDTD-->
<!--
<!ENTITY % paramlangDTD SYSTEM "paramLangDTD.dtd">
%paramlangDTD;
<!ELEMENT croplang (paramlang*) >
-->

<!-- CropDisplayDTD -->
<!--
<!ENTITY % paramdispDTD SYSTEM "paramDisplayDTD.DTD">
%paramdispDTD;
<!ELEMENT cropdisplay (category*) >
<!ELEMENT category (categoryname, paramdisplay* ) >
<!ELEMENT categoryname (#PCDATA)>
-->

<xsl:key name="lang-lookup" match="paramlang" use="paramname"/>
<xsl:key name="display-lookup" match="paramdisplay" use="paramname"/>

<xsl:template match="/">
<html>
<body>
<h3>Crop Format </h3>

<xsl:variable name="lang-top" select="document('crop_lang.xml')/croplang"/>
<xsl:variable name="display-top" select="document('crop_display.xml')/cropdisplay"/>

<table border="1" cellpadding="5">
	<tr>
		<th>Param Prompt</th>
		<th>Param Unit</th>
		<th>Conversion factor</th>
		<th>Alternate units</th>
		<th>Param Choices</th>
<!--
		<th>Param Name</th>
		<th>Param Type</th>
		<th>Param Display</th>
-->

	</tr> 
	<xsl:for-each select="cropdefn/paramdefn">
		<tr>
			
			<td>
				<xsl:apply-templates select="$lang-top">
				<xsl:with-param name="name-for-prompt" select="paramname"/>
				</xsl:apply-templates>
			</td>
				
			<td>
				<xsl:for-each select="paramunit">
				<xsl:value-of select="."/>
				</xsl:for-each>
			</td>	

			<td>
				<xsl:apply-templates select="$lang-top">
				<xsl:with-param name="name-for-conversion" select="paramname"/>
				</xsl:apply-templates>
			</td>

			<td>
				<xsl:apply-templates select="$lang-top">
				<xsl:with-param name="name-for-unit" select="paramname"/>
				</xsl:apply-templates>
			</td>

			<td>
				<xsl:apply-templates select="$lang-top">
				<xsl:with-param name="name-for-choice" select="paramname"/>
			 	</xsl:apply-templates>
			</td>
<!--
			<td>
				<xsl:for-each select="paramname">
				<xsl:value-of select="."/>
				</xsl:for-each>
			</td>
			<td>
				<xsl:for-each select="paramtype">
				<xsl:value-of select="."/>
				</xsl:for-each>
			</td>	
			<td>
				<xsl:apply-templates select="$display-top">
				<xsl:with-param name="name-for-display" select="paramname"/>
				</xsl:apply-templates>
			</td>
-->

		</tr>
	</xsl:for-each>
</table>
</body>
</html>
</xsl:template>

		
<xsl:template match="croplang">
	<xsl:param name="name-for-prompt"/>
	<xsl:param name="name-for-unit"/>
	<xsl:param name="name-for-conversion"/>
	<xsl:param name="name-for-choice"/>
	
	<xsl:value-of select="key('lang-lookup', $name-for-prompt)/paramprompt"/>
	<xsl:for-each select="key('lang-lookup', $name-for-unit)/paramaltunit">
		<!--<xsl:value-of select="key('lang-lookup', $name-for-unit)/paramunit"/>-->
		<xsl:value-of select="."/>
	</xsl:for-each>
	<xsl:for-each select="key('lang-lookup', $name-for-conversion)/conversion">
		<xsl:text>value * </xsl:text>
		<xsl:value-of select="factor"/>
		<xsl:text>+</xsl:text>
		<xsl:value-of select="addend"/>
	</xsl:for-each>
	<xsl:for-each select="key('lang-lookup', $name-for-choice)/paramchoice">
		<!--<xsl:value-of select="key('lang-lookup', $name-for-unit)/paramunit"/>-->
		<xsl:value-of select="@value"/>
		<xsl:text>-</xsl:text>
		<xsl:value-of select="."/><br></br>
	</xsl:for-each>
	<!-- this works because when you call with one param defined, then the other parameter value is null(?), so it has nothing in 'select'!!!!-->
</xsl:template>

<xsl:template match="cropdisplay">
	<xsl:param name="name-for-display"/>
	<xsl:value-of select="key('display-lookup', $name-for-display)/displayattribute"/>
	<xsl:text>,</xsl:text>
	<xsl:value-of select="key('display-lookup', $name-for-display)/valueattribute"/>
</xsl:template>
</xsl:stylesheet>
