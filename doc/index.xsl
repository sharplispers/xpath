<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
	<title>
	  <xsl:value-of select="/page/@title"/>
	</title>
	<link rel="stylesheet" type="text/css" href="index.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
	<xsl:call-template name="sidebar"/>
	<xsl:call-template name="header"/>
	<xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="page">
    <div id="homepage" class="main">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="blau">
    <span style="color: black">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="raute">
    <span style="font-size: 12pt">&#x2b17;</span>
  </xsl:template>

  <xsl:template match="toc">
    <ul>
      <xsl:for-each select="//section">
	<li>
	  <a href="#{generate-id()}">
	    <xsl:apply-templates/>
	  </a>
	</li>
      </xsl:for-each>
    </ul>
  </xsl:template>

  <xsl:template match="section">
    <h3>
      <xsl:apply-templates/>
      <a name="{generate-id()}"/>
    </h3>
  </xsl:template>

  <xsl:template name="header">
    <div id="homepage-header">
      <div style="margin-left: 30px">
	<b>
	  <span class="colored">
	    <xsl:value-of select="/page/@title"/>
	  </span>
	</b>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="sidebar">
    <div class="sidebar">
      <xsl:if test="/page/@clear-sidebar">
	<xsl:attribute name="style">
	  clear: <xsl:value-of select="/page/@clear-sidebar"/>;
	</xsl:attribute>
      </xsl:if>
      <div class="sidebar-title">
	<a href="index.html">
	  <xsl:value-of select="/page/@title"/>
	</a>
      </div>
      <div class="sidebar-main">
	<ul class="main">
	  <li>
	    <a href="installation.html">
	      Download and Installation
	    </a>
	  </li>
	  <li>
	    <a href="examples.html">Examples</a>
	  </li>
	  <li>
	    <a href="atdoc/index.html">API documentation</a>
          </li>
	</ul>
      </div>
    </div>
  </xsl:template>
</xsl:stylesheet>
