<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="core">
    <html>
    <head>
        <meta charset="iso-8859-1"/>
        <link rel="icon" href="/images/favicon.ico" type="image/x-icon"/>
        <title>
            <xsl:value-of select="title"/>
        </title>
    </head>
<body>
    <h2><xsl:value-of select="headline"/></h2>
    <p>
        <a href="{goback/@url}"><xsl:value-of select="goback"/></a>
    </p>
</body>
</html>
</xsl:template>

</xsl:stylesheet>
