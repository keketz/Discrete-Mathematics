Public Class Form1
    Dim ProblemText As String
    Dim counter As Integer = 0
    Dim compareSets(8) As Char


    'This sets up the table and prints The Sets T or F in the Correct Place (It's just for Set A, B, C, D, Not for The equation)
    Private Sub SetTable(TextInput As String)
        If TextInput.Contains("a") Then
            For columnA = 0 To 1

                If TextInput.Contains("b") Then
                    For columnB = 0 To 1

                        If TextInput.Contains("c") Then
                            For columnC = 0 To 1

                                If TextInput.Contains("d") Then
                                    For columnD = 0 To 1
                                        IfSetsAreTrueOrFalse(columnA, columnB, columnC, columnD)
                                    Next columnD
                                Else
                                    IfSetsAreTrueOrFalse(columnA, columnB, columnC, 2)
                                End If

                            Next columnC
                        Else
                            IfSetsAreTrueOrFalse(columnA, columnB, 2, 2)
                        End If

                    Next columnB
                Else
                    IfSetsAreTrueOrFalse(columnA, 2, 2, 2)
                End If

            Next columnA
        End If

    End Sub

    'Determines if a Set is True or False
    Private Sub IfSetsAreTrueOrFalse(columnA As Integer, columnB As Integer, columnC As Integer, columnD As Integer)
        If columnA = 0 Then
            TruthBoxA.AppendText("T" + vbNewLine)
        ElseIf columnA = 1
            TruthBoxA.AppendText("F" + vbNewLine)
        End If

        If columnB = 0 Then
            TruthBoxB.AppendText("T" + vbNewLine)
        ElseIf columnB = 1
            TruthBoxB.AppendText("F" + vbNewLine)
        End If

        If columnC = 0 Then
            TruthBoxC.AppendText("T" + vbNewLine)
        ElseIf columnC = 1
            TruthBoxC.AppendText("F" + vbNewLine)
        End If

        If columnD = 0 Then
            TruthBoxD.AppendText("T" + vbNewLine)
        ElseIf columnD = 1
            TruthBoxD.AppendText("F" + vbNewLine)
        End If


    End Sub

    'Erases Text from All Feilds
    Private Sub ClearText()
        TruthBoxA.Text = ""
        TruthBoxB.Text = ""
        TruthBoxC.Text = ""
        TruthBoxD.Text = ""
        LabelEquation.Text = ""
        TruthBoxEquation.Text = ""
        EquationsCompleteTruth.Text = ""

        For eraseCounter = 0 To 7
            compareSets(eraseCounter) = ""
        Next eraseCounter

    End Sub

    Private Sub Problem1_Click(sender As Object, e As EventArgs) Handles Problem1.Click
        ClearText()
        ProblemText = "(a ⋁ (¬a ⋀ (¬b ⋁ b))) ⋀ b"
        SetTable(ProblemText)
        LabelEquation.Text = "(a ⋁ (¬a ⋀ (¬b ⋁ b))) ⋀ b"

        'Nifty Thing that Sizes the Text Box to the Same Size as The Equation in the Label
        TruthBoxEquation.Width = LabelEquation.Width

        'Me being tired Of Arrays being weird.
        Dim TruthTextA() As Char = "TTFF"
        Dim TruthTextB() As Char = "TFTF"

        'Counts Fromt Left To Right
        'Displays T or F depending on Criteria
        For setCounter = 0 To 3
            'True if B is T or F
            If TruthTextB(setCounter) = "T" Or TruthTextB(setCounter) = "F" Then
                compareSets(1) = "T"
            Else
                compareSets(1) = "F"
            End If

            'True if A is F and B is T or F
            If TruthTextA(setCounter) = "F" And compareSets(1) = "T" Then
                compareSets(2) = "T"
            Else
                compareSets(2) = "F"
            End If

            'True if A is T and (A is F and B is T or F)
            If TruthTextA(setCounter) = "T" Or compareSets(2) = "T" Then
                compareSets(3) = "T"
            Else
                compareSets(3) = "F"
            End If

            'True if (A is T and (A is F and B is T or F)) and B is T
            If compareSets(3) = "T" And TruthTextB(setCounter) = "T" Then
                EquationsCompleteTruth.AppendText("T" + vbNewLine)
                compareSets(4) = "T"
            Else
                EquationsCompleteTruth.AppendText("F" + vbNewLine)
                compareSets(4) = "F"
            End If

            'Prints T or F in the correct Spot
            TruthBoxEquation.AppendText("     (" + compareSets(3) + "           (" + compareSets(2) + "           (" + compareSets(1) + ")))         " + compareSets(4))
            TruthBoxEquation.AppendText(vbNewLine)
        Next setCounter
    End Sub

    Private Sub Problem2_Click(sender As Object, e As EventArgs) Handles Problem2.Click
        ClearText()
        ProblemText = "¬(a⋀b⋁c)⋀(a⋁(b⋀¬c))"
        SetTable(ProblemText)
        LabelEquation.Text = "¬(a ⋁ b ⋀ c) ⋁ (a ⋁ (b ⋀ ¬c))"
        TruthBoxEquation.Width = LabelEquation.Width

        'Me being tired Of Arrays being weird.
        Dim TruthTextA() As Char = "TTTTFFFF"
        Dim TruthTextB() As Char = "TTFFTTFF"
        Dim TruthTextC() As Char = "TFTFTFTF"

        For setCounter = 0 To 7
            'True if B is T and C is F
            If TruthTextB(setCounter) = "T" And TruthTextC(setCounter) = "F" Then
                compareSets(1) = "T"
            Else
                compareSets(1) = "F"
            End If

            'True if A is T and (B is T and C is F)
            If TruthTextA(setCounter) = "T" Or compareSets(1) = "T" Then
                compareSets(2) = "T"
            Else
                compareSets(2) = "F"
            End If

            'True if A is T and B is T or if C is T
            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T" Or TruthTextC(setCounter) = "T" Then
                compareSets(3) = "T"
            Else
                compareSets(3) = "F"
            End If

            'True if (A is T and B is T or if C is T) and (A is T and (B is T and C is F))
            If compareSets(3) = "F" And compareSets(2) = "T" Then
                EquationsCompleteTruth.AppendText("T" + vbNewLine)
                compareSets(4) = "T"
            Else
                EquationsCompleteTruth.AppendText("F" + vbNewLine)
                compareSets(4) = "F"
            End If

            'Prints T or F in the correct Spot
            TruthBoxEquation.AppendText("              (" + compareSets(3) + ")           " + compareSets(4) + "         (" + compareSets(2) + "        (" + compareSets(1) + "))")
            TruthBoxEquation.AppendText(vbNewLine)
        Next setCounter

    End Sub

    Private Sub Problem3_Click(sender As Object, e As EventArgs) Handles Problem3.Click
        ClearText()
        ProblemText = "(a⋁b)⋀(a⋀c⋁a⋀¬c)⋁a⋀b⋁b"
        SetTable(ProblemText)
        LabelEquation.Text = "(a ⋁ b) ⋀ (a ⋀ c ⋁ a ⋀ ¬c) ⋁ a ⋀ b ⋁ b"
        TruthBoxEquation.Width = LabelEquation.Width

        'Me being tired Of Arrays being weird.
        Dim TruthTextA() As Char = "TTTTFFFF"
        Dim TruthTextB() As Char = "TTFFTTFF"
        Dim TruthTextC() As Char = "TFTFTFTF"

        For setCounter = 0 To 7
            'If A or B Are True
            If TruthTextA(setCounter) = "T" Or TruthTextB(setCounter) = "T" Then
                compareSets(1) = "T"
            Else
                compareSets(1) = "F"
            End If

            'True if A and C ar T or if A is T and C is F
            If (TruthTextA(setCounter) = "T" And TruthTextC(setCounter) = "T") Or (TruthTextA(setCounter) = "T" And TruthTextC(setCounter) = "F") Then
                compareSets(2) = "T"
            Else
                compareSets(2) = "F"
            End If

            'If A and B are True
            If (TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T") Then
                compareSets(3) = "T"
            Else
                compareSets(3) = "F"
            End If

            'If A or B are True AND if ((A and C are True) or (A is True and C is False))
            '   compareSets(1)  AND    CompareSets(2)
            If compareSets(1) = "T" And compareSets(2) = "T" Then
                compareSets(4) = "T"
            Else
                compareSets(4) = "F"
            End If

            ' True if (A or B are True AND if ((A and C are True) or (A is True and C is False))) OR A dn B are T or B is T
            If compareSets(4) = "T" Or compareSets(3) = "T" Or TruthTextB(setCounter) = "T" Then
                EquationsCompleteTruth.AppendText("T" + vbNewLine)
            Else
                EquationsCompleteTruth.AppendText("F" + vbNewLine)
            End If

            'Prints T or F in the correct Spot
            TruthBoxEquation.AppendText("      (" + compareSets(1) + ")       " + compareSets(4) + "                 (" + compareSets(2) + ")                            " + compareSets(3))
            TruthBoxEquation.AppendText(vbNewLine)
        Next setCounter
    End Sub

    Private Sub Problem4_Click(sender As Object, e As EventArgs) Handles Problem4.Click
        ClearText()
        ProblemText = "(a⋀b⋀c⋀d)⋁(a⋀b⋀c⋀¬d)⋁(a⋀b⋀¬c⋀d)⋁(a⋀b⋀¬c⋀¬d)⋁(a⋀¬b⋀c⋀d)⋁(a⋀¬b⋀c⋀¬d)⋁(a⋀¬b⋀d)"
        SetTable(ProblemText)
        LabelEquation.Text = "(a ⋀ b ⋀ c ⋀ d) ⋁ (a ⋀ b ⋀ c ⋀ ¬d) ⋁ (a ⋀ b ⋀ ¬c ⋀ d) ⋁ (a ⋀ b ⋀ ¬c ⋀ ¬d) ⋁ (a ⋀ ¬b ⋀ c ⋀ d) ⋁ (a ⋀ ¬b ⋀ c ⋀ ¬d) ⋁ (a ⋀ ¬b ⋀ d)"
        TruthBoxEquation.Width = LabelEquation.Width

        'Me being tired Of Arrays being weird.
        Dim TruthTextA() As Char = "TTTTTTTTFFFFFFFF"
        Dim TruthTextB() As Char = "TTTTFFFFTTTTFFFF"
        Dim TruthTextC() As Char = "TTFFTTFFTTFFTTFF"
        Dim TruthTextD() As Char = "TFTFTFTFTFTFTFTF"

        'Counts Fromt Left To Right
        'Displays T or F depending on Criteria
        For setCounter = 0 To 15
            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T" And TruthTextC(setCounter) = "T" And TruthTextD(setCounter) = "T" Then
                compareSets(1) = "T"
            Else
                compareSets(1) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T" And TruthTextC(setCounter) = "T" And TruthTextD(setCounter) = "F" Then
                compareSets(2) = "T"
            Else
                compareSets(2) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T" And TruthTextC(setCounter) = "F" And TruthTextD(setCounter) = "T" Then
                compareSets(3) = "T"
            Else
                compareSets(3) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "T" And TruthTextC(setCounter) = "F" And TruthTextD(setCounter) = "F" Then
                compareSets(4) = "T"
            Else
                compareSets(4) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "F" And TruthTextC(setCounter) = "T" And TruthTextD(setCounter) = "T" Then
                compareSets(5) = "T"
            Else
                compareSets(5) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "F" And TruthTextC(setCounter) = "T" And TruthTextD(setCounter) = "F" Then
                compareSets(6) = "T"
            Else
                compareSets(6) = "F"
            End If

            If TruthTextA(setCounter) = "T" And TruthTextB(setCounter) = "B" And TruthTextD(setCounter) = "T" Then
                compareSets(7) = "T"
            Else
                compareSets(7) = "F"
            End If
            If compareSets(1) = "T" Or compareSets(2) = "T" Or compareSets(3) = "T" Or compareSets(4) = "T" Or compareSets(5) = "T" Or compareSets(6) = "T" Or compareSets(7) = "T" Then
                EquationsCompleteTruth.AppendText("T" + vbNewLine)
            Else
                EquationsCompleteTruth.AppendText("T" + vbNewLine)
            End If
            TruthBoxEquation.AppendText("              (" + compareSets(1) + ")                                    (" + compareSets(2) + ")                                        (" + compareSets(3) + ")                                      (" + compareSets(4) + ")                                              (" + compareSets(5) + ")                                      (" + compareSets(6) + ")                                    (" + compareSets(7) + ")")
            TruthBoxEquation.AppendText(vbNewLine)
        Next setCounter
    End Sub
End Class

