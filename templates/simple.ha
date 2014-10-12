<?php
{{{namespace}}}{{{imports}}}{{{interfaceImports}}}{{{parentImports}}}
class {{className}}{{extends}}{{implements}}
{
{{#properties}}
    private ${{name}};
{{/properties}}

    public function __construct({{argumentsList}})
    {
{{#properties}}
        $this->{{name}} = ${{name}};
{{/properties}}
    }
{{#properties}}

    public function {{getterName}}()
    {
        return $this->{{name}};
    }
{{/properties}}
}