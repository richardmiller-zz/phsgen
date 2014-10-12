<?php
{{{namespace}}}{{{imports}}}
class {{className}}
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

    public function {{name}}()
    {
        return $this->{{name}};
    }
{{/properties}}
}