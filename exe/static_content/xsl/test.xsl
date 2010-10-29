<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="core">
    <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <meta http-equiv="Content-Type" content="application/xhtml+xml; charset=iso-8859-1" />
        <title>
            <xsl:value-of select="title"/>
        </title>
    </head>
<body>
    <h2><xsl:value-of select="headline"/></h2>
    <a href="{goback/@url}"><xsl:value-of select="goback"/></a>
</body>
</html>
</xsl:template>

</xsl:stylesheet>