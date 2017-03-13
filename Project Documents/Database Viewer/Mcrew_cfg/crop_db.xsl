<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- cropDB.DTD - provided here fore reference
<!ELEMENT cropDB (cropname, param*)>
<!ELEMENT cropname (#PCDATA)>
<!ELEMENT param (name,value)>
<!ELEMENT name (#PCDATA)>
<!ELEMENT value (#PCDATA)>
-->

<xsl:template match="/">
<html>
<body>
<h2>Crop Operation DB</h2>
<xsl:for-each select="cropDB">
	<!-- <h3>Date: <xsl:value-of select="date"/></h3>-->

	<h3><xsl:value-of select="cropname"/></h3>
	<table border="1">
		<tr>
			<th>Paramname</th>
			<th>ParamValue</th>
		</tr>
		<xsl:for-each select="param">
			<tr>
				<td><xsl:value-of select="name"/></td>
				<td><xsl:value-of select="value"/></td>
			</tr>
		</xsl:for-each>
	</table>
</xsl:for-each>
</body>
</html>

</xsl:template>
</xsl:stylesheet>
			