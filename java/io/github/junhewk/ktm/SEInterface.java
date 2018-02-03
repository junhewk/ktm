package io.github.junhewk.ktm;

import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import scala.collection.JavaConversions;

import org.bitbucket.eunjeon.seunjeon.Analyzer;
import org.bitbucket.eunjeon.seunjeon.LNode;
import org.bitbucket.eunjeon.seunjeon.Eojeol;
import org.bitbucket.eunjeon.seunjeon.Morpheme;
import org.bitbucket.eunjeon.seunjeon.MorphemeType;

public class SEInterface {

    public String[] tagger(String sentence, String separator) {

        List<String> list = null;

        list = new ArrayList<String>();

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            list.add(morpheme.surface() + separator + morpheme.feature().head());
        }

        return list.toArray(new String[0]);

    }



    public String[] nounTagger(String sentence, String separator) {

        List<String> list = null;

        list = new ArrayList<String>();

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            list.add(morpheme.surface() + separator + morpheme.feature().head());
        }

        return list.toArray(new String[0]);

    }

    public String[] unpackedTagger(String sentence) {

        List<String> ret = new ArrayList<String>();
        String residue = "/*";

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            if (morpheme.mType() == MorphemeType.INFLECT()) {
                List<String> features = JavaConversions.seqAsJavaList(morpheme.feature());
                String[] combined = features.get(7).split("\\+");
                for (String elements: combined) {
                    ret.add(elements.replace(residue, ""));
                }
            } else {
                ret.add(morpheme.surface() + "/" + morpheme.feature().head());
            }
        }

        return ret.toArray(new String[0]);

    }

    public String[] unpackedTaggerSep(String sentence, String separator) {

        List<String> ret = new ArrayList<String>();
        String residue = "/*";

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            if (morpheme.mType() == MorphemeType.INFLECT()) {
                List<String> features = JavaConversions.seqAsJavaList(morpheme.feature());
                String[] combined = features.get(7).split("\\+");
                for (String elements: combined) {
                    String tagged = elements.replace(residue, "");
                    ret.add(tagged.replace("/", separator));
                }
            } else {
                ret.add(morpheme.surface() + separator + morpheme.feature().head());
            }
        }

        return ret.toArray(new String[0]);

    }

    public String[] tokenMorpheme(String sentence) {

        List<String> list = null;

        list = new ArrayList<String>();

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            list.add(morpheme.surface());
        }

        return list.toArray(new String[0]);

    }

    public String[] tokenNoun(String sentence) {

        List<String> ret = new ArrayList<String>();

        for (LNode node: Analyzer.parseJava(sentence)) {
            Morpheme morpheme = node.morpheme();
            if (morpheme.poses().head().toString() == "N") {
                ret.add(morpheme.surface());
            }
        }

        return ret.toArray(new String[0]);

    }

    public String[] taggerDeinflect(String sentence, String separator) {

        List<String> list = null;

        list = new ArrayList<String>();

        for (LNode node : Analyzer.parseJava(sentence)) {
            for (LNode node2: node.deInflectJava()) {
                Morpheme morpheme = node2.morpheme();
                list.add(morpheme.surface() + separator + morpheme.feature().head());
            }
        }
        return list.toArray(new String[0]);

    }

    public void setDict(String[] dictArray) {
        
        Analyzer.setUserDict(Arrays.asList(dictArray).iterator());

        return;
    }

    public void resetDict() {
        
        Analyzer.resetUserDict();

        return;
    }



    public static void main(String[] args) {
        // 형태소 분석
        for (LNode node : Analyzer.parseJava("아버지가방에들어가신다.")) {
            System.out.println(node);
        }

        // 어절 분석
        for (Eojeol eojeol: Analyzer.parseEojeolJava("아버지가방에들어가신다.")) {
            System.out.println(eojeol);
            for (LNode node: eojeol.nodesJava()) {
                System.out.println(node);
            }
        }

        /**
         * 사용자 사전 추가
         * surface,cost
         *   surface: 단어명. '+' 로 복합명사를 구성할 수 있다.
         *           '+'문자 자체를 사전에 등록하기 위해서는 '\+'로 입력. 예를 들어 'C\+\+'
         *   cost: 단어 출연 비용. 작을수록 출연할 확률이 높다.
         */
        Analyzer.setUserDict(Arrays.asList("덕후", "버카충,-100", "낄끼+빠빠,-100").iterator());
        for (LNode node : Analyzer.parseJava("덕후냄새가 난다.")) {
            System.out.println(node);
        }

        // 활용어 원형
        for (LNode node : Analyzer.parseJava("빨라짐")) {
            for (LNode node2: node.deInflectJava()) {
                System.out.println(node2);
            }
        }

        // 복합명사 분해
        for (LNode node : Analyzer.parseJava("낄끼빠빠")) {
            System.out.println(node);   // 낄끼빠빠
            for (LNode node2: node.deCompoundJava()) {
                System.out.println(node2);  // 낄끼+빠빠
            }
        }
    }

    
}