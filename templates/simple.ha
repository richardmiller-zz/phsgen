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
{{#serializationMethods}}

    public function serialize()
    {
        return [
{{#properties}}
            '${{name}}' => $this->{{name}},
{{/properties}}
        ];
    }

    public static function deserialize(array $data)
    {
        return new self({{{deserializationArguments}}});
    }
{{/serializationMethods}}
}