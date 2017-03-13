<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--wepsmanDB.dtd: Provided here for reference-->
<!--
<!ENTITY % operationDBDTD SYSTEM "operation_db.dtd">
%operationDBDTD;
<!ELEMENT wepsmanDB (wepsmanvalue*)>
<!ELEMENT wepsmanvalue (date, operationDB*)>
<!ELEMENT date (#PCDATA)>
-->

<!-- operationDB.dtd -->
<!--
<!ENTITY % identityDTD SYSTEM "identity.dtd">
%identityDTD;
<!ELEMENT operationDB (operationname, actionvalue*)>
<!ELEMENT operationname (#PCDATA)>
<!ELEMENT actionvalue (identity, param*)>
<!ELEMENT param (name,value)>
<!ELEMENT name (#PCDATA)>
<!ELEMENT value (#PCDATA)>

-->

<xsl:key name="defn-lookup" match="action" use="identity"/>

<xsl:template match="/">
<xsl:variable name="defn-top" select="document('operation_defn.xml')/operationdefn"/>

<html>
<body>
<h2>Operation Database</h2>
<xsl:for-each select="wepsmanDB/wepsmanvalue">
	<h3><xsl:value-of select="date"/></h3>
	<xsl:for-each select="operationDB">
		<xsl:value-of select="operationname"/>
	<table border="1" cellpadding="5">
		<tr>
			<th>Id</th>
			<th>Code</th>
			<th>TypeName</th>
			<th>Values</th>
		</tr>
		<xsl:for-each select="actionvalue">
			<tr>
				<td><xsl:value-of select="identity/id"/></td>
				<td><xsl:value-of select="identity/code"/></td>
				<td>
					<xsl:apply-templates select="$defn-top">
				   <xsl:with-param name="identity-for-actionname" select="identity"/>
				  </xsl:apply-templates>
				</td>
				<td>
					<table border="0" cellpadding="5">
						<tr>
							<xsl:for-each select= "param">
								<th><xsl:value-of select="name"/></th>
							</xsl:for-each>
						</tr>
						<tr>
							<xsl:for-each select= "param">
								<th><xsl:value-of select="value"/></th>
							</xsl:for-each>
						</tr>
					</table>
				</td>
			</tr>
		</xsl:for-each><!--end of for-each operationDB-->
	</table>
	</xsl:for-each>
	</xsl:for-each>
</body>
</html>						
</xsl:template>

<xsl:template match="operationdefn">
	<xsl:param name="identity-for-actionname"/>
		<xsl:value-of select="key('defn-lookup', $identity-for-actionname)/actionname"/>
</xsl:template>

</xsl:stylesheet>								
