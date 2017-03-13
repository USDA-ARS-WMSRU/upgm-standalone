<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- mcrewconfig.dtd-->
<!--
<!ELEMENT mcrewconfig <dataobject*, columndefn*>
	<!ELEMENT dataobjects (objectname, defnfile, displayfile, langfile, DBDir, tagname)>
	<!ATTLIST dataobjects type CDATA "control">
		<!ELEMENT objectname (#PCDATA)>
		<!ELEMENT defnfile (#CDATA)>
		<!ELEMENT displayfile (#PCDATA)>
		<!ELEMENT langfile (#PCDATA)>
		<!ELEMENT DBdir (#PCDATA)
		<!ELEMENT tagname (#PCDATA)>
	<!ELEMENT columndefn (columnnum, columndataobject, columnlabel)>
		<!ELEMENT columnnum (#PCDATA)>
		<!ELEMENT columndataobject (#PCDATA)>
		<!ELEMENT columnlabel (#PCADATA)		
-->

<xsl:key name="columndefn-lookup" match="columndefn" use="columndataobject"/>

<xsl:template match="/">
<html>
<body>

<h3>Configuration File</h3>
<table border="1" cellpadding="5">
	<tr>
		<th>ObjectName</th>
		<th>Defn File</th>
		<th>Display File</th>
		<th>Lang File</th>
		<th>DBdir</th>
		<th>Data to Col</th>
	</tr>
	<xsl:for-each select="mcrewconfig/dataobject">
		<tr>
			<td><xsl:value-of select="objectname"/></td>
			<td><xsl:value-of select="defnfile"/></td>
			<td><xsl:value-of select="displayfile"/></td>
			<td><xsl:value-of select="langfile"/></td>
			<td><xsl:value-of select="DBdir"/></td>
			<td><xsl:apply-templates select=".">
				<xsl:with-param name="name-for-columndefn" select="objectname"/>
				</xsl:apply-templates>
			</td>
		</tr>
	</xsl:for-each>
</table>

</body>
</html>
</xsl:template>
		
<xsl:template match="dataobject">
	<xsl:param name="name-for-columndefn"/>
		<xsl:value-of select="key('columndefn-lookup',$name-for-columndefn)/columnnum"/>
</xsl:template>

</xsl:stylesheet>