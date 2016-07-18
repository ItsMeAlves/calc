class Expression
    @value = nil
    @left = nil
    @right = nil

    def translate(s)
        @value = s[0]
    end
end