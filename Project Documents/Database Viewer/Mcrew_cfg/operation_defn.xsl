<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- operationDefnDTD -->
<!--
<!ELEMENT operationdefn (action*) >
<!ENTITY % identityDTD SYSTEM "identityDTD.dtd">
%identityDTD;
<!ENTITY % paramdefnDTD SYSTEM "paramDefnDTD.dtd">
%paramdefnDTD;
<!ELEMENT action (identity, actionname, (paramdefn*))>
<!ELEMENT actionname (#PCDATA)>
-->

<!-- opearion_lang.DTD-->
<!--
<!ELEMENT operationlang (action*) >
<!ENTITY % identityDTD SYSTEM "identityDTD.dtd">
%identityDTD;
<!ENTITY % paramlangDTD SYSTEM "paramLangDTD.dtd">
%paramlangDTD;
<!ELEMENT action (identity, actionname, (paramlang*))>
<!ELEMENT actionname (#PCDATA)>
-->

<!-- operationDisplayDTD -->
<!--
<!ELEMENT operationdisplay (action*) >
<!ENTITY % identityDTD SYSTEM "identityDTD.dtd">
%identityDTD;
<!ENTITY % paramdispDTD SYSTEM "paramDisplayDTD.DTD">
%paramdispDTD;
<!ELEMENT action (identity , paramdisplay*)>
-->

<xsl:key name="lang-lookup" match="paramlang" use="paramname"/>
<xsl:key name="display-lookup" match="paramdisplay" use="paramname"/>

<xsl:template match="/">
<html>
<body>
<h3>Operation Format </h3>

<xsl:variable name="lang-top" select="document('operation_lang.xml')/operationlang"/>
<xsl:variable name="display-top" select="document('operation_display.xml')/operationdisplay"/>

<table border="1" fgcolor="#00cc00" cellpadding="5">
	<tr>
		<th>Code</th>
		<th>Id</th>
		<th>Action Name</th>
		<td><xsl:text>Parm Prompt, Alternate Units and Param Choices are from operation_lang.xml file</xsl:text>
			<br><xsl:text> Param Display is from operation_display.xml file</xsl:text></br>
		</td>			
	</tr>
	<xsl:for-each select="operationdefn/action">
	<tr>
		<td><xsl:value-of select="identity/code"/></td>
		<td><xsl:value-of select="identity/id"/></td>
		<td><xsl:value-of select="actionname"/></td>
	
		<td>
			<table border="1" cellpadding="5">
				<tr>
					<th>Param Name</th>
					<th>Param Type</th>
					<th>Param Unit</th>
					<th>Alternate units</th>
					<th>Conversion</th>
					<th>Param Prompt</th>
					<th>Param Choices</th>
					<th>Param Display</th>
				</tr> 
				<xsl:for-each select="paramdefn">
				<tr>
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
						<xsl:for-each select="paramunit">
							<xsl:value-of select="."/>
						</xsl:for-each>
					</td>	
		
					<td>
						<xsl:apply-templates select="$lang-top">
					   <xsl:with-param name="name-for-unit" select="paramname"/>
					  </xsl:apply-templates>
					</td>
				
					<td>
						<xsl:apply-templates select="$lang-top">
					   <xsl:with-param name="name-for-conversion" select="paramname"/>
					  </xsl:apply-templates>
					</td>
				
					<td>
						<xsl:apply-templates select="$lang-top">
					   <xsl:with-param name="name-for-prompt" select="paramname"/>
					  </xsl:apply-templates>
					</td>
					
					<td>
						<xsl:apply-templates select="$lang-top">
					   <xsl:with-param name="name-for-choice" select="paramname"/>
					  </xsl:apply-templates>
					</td>

					<td>
						<xsl:apply-templates select="$display-top">
					   <xsl:with-param name="name-for-display" select="paramname"/>
					  </xsl:apply-templates>
					</td>
				</tr>
				</xsl:for-each>
			</table>
		</td>	
	</tr>
	</xsl:for-each>
</table>
</body>
</html>
</xsl:template>

		
	<xsl:template match="operationlang">
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
				<!--<xsl:value-of select="key('lang-lookup', $name-for-unit)/paramunit"/>-->
			<xsl:text>value *</xsl:text>
			<xsl:value-of select="factor"/>
			<xsl:text>+</xsl:text>
			<xsl:value-of select="addend"/>
		</xsl:for-each>
		
		<xsl:for-each select="key('lang-lookup', $name-for-choice)/paramchoice">
				<!--<xsl:value-of select="key('lang-lookup', $name-for-unit)/paramunit"/>-->
			<xsl:value-of select="."/>
			<xsl:text>:</xsl:text>
			<xsl:value-of select="@value"/><br></br>
		</xsl:for-each>
		<!-- this works because when you call with one param defined, then the other parameter value is null(?), so it has nothing in 'select'!!!!-->
</xsl:template>

<xsl:template match="operationdisplay">
	<xsl:param name="name-for-display"/>
		<xsl:value-of select="key('display-lookup', $name-for-display)/displayattribute"/>
		<xsl:text>,</xsl:text>
		<xsl:value-of select="key('display-lookup', $name-for-display)/valueattribute"/>
</xsl:template>
</xsl:stylesheet>
